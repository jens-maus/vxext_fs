/* vim:set ts=2 nowrap: ****************************************************

 VXEXT fs - VxWorks extended DOS filesystem support
 Copyright (c) 2004-2005 by Jens Langner <Jens.Langner@light-speed.de>

 This filesystem module is a reverse engineered implementation of the so
 called VXEXT1.0 extended DOS filesystem shipped with the VxWorks 5.2+
 RTOS operating system. The sources are largly based on the FAT and MSDOS
 filesystem routines found in the main Linux kernel sources which are
 copyright by their respecitive authors. However, minor cosmetic changes
 have been made and non-required parts were removed wherever possible.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 $Id$

***************************************************************************/

#include <linux/module.h>
#include <linux/time.h>
#include <linux/slab.h>
#include <linux/smp_lock.h>
#include <linux/seq_file.h>
#include <linux/vxext_fs.h>
#include <linux/pagemap.h>
#include <linux/buffer_head.h>
#include <linux/mount.h>
#include <linux/vfs.h>
#include <linux/parser.h>
#include <asm/unaligned.h>

/*
 * New FAT inode stuff. We do the following:
 *	a) i_ino is constant and has nothing with on-disk location.
 *	b) FAT manages its own cache of directory entries.
 *	c) *This* cache is indexed by on-disk location.
 *	d) inode has an associated directory entry, all right, but
 *		it may be unhashed.
 *	e) currently entries are stored within struct inode. That should
 *		change.
 *	f) we deal with races in the following way:
 *		1. readdir() and lookup() do FAT-dir-cache lookup.
 *		2. rename() unhashes the F-d-c entry and rehashes it in
 *			a new place.
 *		3. unlink() and rmdir() unhash F-d-c entry.
 *		4. vxext_write_inode() checks whether the thing is unhashed.
 *			If it is we silently return. If it isn't we do bread(),
 *			check if the location is still valid and retry if it
 *			isn't. Otherwise we do changes.
 *		5. Spinlock is used to protect hash/unhash/location check/lookup
 *		6. vxext_clear_inode() unhashes the F-d-c entry.
 *		7. lookup() and readdir() do igrab() if they find a F-d-c entry
 *			and consider negative result as cache miss.
 */

#define FAT_HASH_BITS	8
#define FAT_HASH_SIZE	(1UL << FAT_HASH_BITS)
#define FAT_HASH_MASK	(FAT_HASH_SIZE-1)
static struct list_head vxext_inode_hashtable[FAT_HASH_SIZE];
spinlock_t vxext_inode_lock = SPIN_LOCK_UNLOCKED;

void vxext_hash_init(void)
{
	int i;

	for(i = 0; i < FAT_HASH_SIZE; i++)
	{
		INIT_LIST_HEAD(&vxext_inode_hashtable[i]);
	}
}

static inline unsigned long vxext_hash(struct super_block *sb, loff_t i_pos)
{
	unsigned long tmp = (unsigned long)i_pos | (unsigned long) sb;
	tmp = tmp + (tmp >> FAT_HASH_BITS) + (tmp >> FAT_HASH_BITS * 2);
	return tmp & FAT_HASH_MASK;
}

void vxext_attach(struct inode *inode, loff_t i_pos)
{
	spin_lock(&vxext_inode_lock);
	VXEXT_I(inode)->i_pos = i_pos;
	list_add(&VXEXT_I(inode)->i_fat_hash,
		vxext_inode_hashtable + vxext_hash(inode->i_sb, i_pos));
	spin_unlock(&vxext_inode_lock);
}

void vxext_detach(struct inode *inode)
{
	spin_lock(&vxext_inode_lock);
	VXEXT_I(inode)->i_pos = 0;
	list_del_init(&VXEXT_I(inode)->i_fat_hash);
	spin_unlock(&vxext_inode_lock);
}

struct inode *vxext_iget(struct super_block *sb, loff_t i_pos)
{
	struct list_head *p;
	struct list_head *walk = NULL;
	struct vxext_inode_info *i;
	struct inode *inode = NULL;

	p = vxext_inode_hashtable + vxext_hash(sb, i_pos);

	spin_lock(&vxext_inode_lock);

	list_for_each(walk, p)
	{
		i = list_entry(walk, struct vxext_inode_info, i_fat_hash);

		if(i->vfs_inode.i_sb != sb || i->i_pos != i_pos)
			continue;

		inode = igrab(&i->vfs_inode);
		if(inode)
			break;
	}

	spin_unlock(&vxext_inode_lock);
	return inode;
}

static int vxext_fill_inode(struct inode *inode, struct vxext_dir_entry *de);

struct inode *vxext_build_inode(struct super_block *sb,
																struct vxext_dir_entry *de,
																loff_t i_pos, int *res)
{
	struct inode *inode;
	*res = 0;
	inode = vxext_iget(sb, i_pos);
	if(inode)
		goto out;

	inode = new_inode(sb);
	*res = -ENOMEM;
	if(!inode)
		goto out;

	inode->i_ino = iunique(sb, VXEXT_ROOT_INO);
	inode->i_version = 1;
	*res = vxext_fill_inode(inode, de);
	if(*res < 0)
	{
		iput(inode);
		inode = NULL;
		goto out;
	}

	vxext_attach(inode, i_pos);
	insert_inode_hash(inode);

out:
	return inode;
}

void vxext_delete_inode(struct inode *inode)
{
	if(!is_bad_inode(inode))
	{
		inode->i_size = 0;
		vxext_truncate(inode);
	}

	clear_inode(inode);
}

void vxext_clear_inode(struct inode *inode)
{
	if(is_bad_inode(inode))
		return;

	lock_kernel();
	spin_lock(&vxext_inode_lock);
	vxext_cache_inval_inode(inode);
	list_del_init(&VXEXT_I(inode)->i_fat_hash);
	spin_unlock(&vxext_inode_lock);
	unlock_kernel();
}

void vxext_put_super(struct super_block *sb)
{
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	sb->s_fs_info = NULL;
	kfree(sbi);
}

static int vxext_show_options(struct seq_file *m, struct vfsmount *mnt)
{
	struct vxext_sb_info *sbi = VXEXT_SB(mnt->mnt_sb);
	struct vxext_mount_options *opts = &sbi->options;

	if(opts->fs_uid != 0)
		seq_printf(m, ",uid=%u", opts->fs_uid);

	if(opts->fs_gid != 0)
		seq_printf(m, ",gid=%u", opts->fs_gid);

	seq_printf(m, ",fmask=%04o", opts->fs_fmask);
	seq_printf(m, ",dmask=%04o", opts->fs_dmask);

	if(opts->quiet)
		seq_puts(m, ",quiet");

	return 0;
}

enum
{
	Opt_uid, Opt_gid, Opt_umask, Opt_dmask, Opt_fmask, Opt_quiet, Opt_debug,
	Opt_uni_xl_no, Opt_uni_xl_yes,
	Opt_err,
};

static match_table_t vxext_tokens =
{
	{Opt_uid, "uid=%u"},
	{Opt_gid, "gid=%u"},
	{Opt_umask, "umask=%o"},
	{Opt_dmask, "dmask=%o"},
	{Opt_fmask, "fmask=%o"},
	{Opt_quiet, "quiet"},
	{Opt_debug, "debug"},
	{Opt_err, NULL}
};

static int parse_options(char *options, int *debug,
												 struct vxext_mount_options *opts)
{
	char *p;
	substring_t args[MAX_OPT_ARGS];
	int option;

	opts->fs_uid = current->uid;
	opts->fs_gid = current->gid;
	opts->fs_fmask = opts->fs_dmask = current->fs->umask;
	opts->quiet = 0;
	*debug = 0;

	if(!options)
		return 1;

	while((p = strsep(&options, ",")) != NULL)
	{
		int token;

		if(!*p)
			continue;

		token = match_token(p, vxext_tokens, args);
		switch(token)
		{
			case Opt_quiet:
				opts->quiet = 1;
			break;
		
			case Opt_debug:
				*debug = 1;
			break;
		
			case Opt_uid:
				if(match_int(&args[0], &option))
					return 0;
				opts->fs_uid = option;
			break;
		
			case Opt_gid:
				if(match_int(&args[0], &option))
					return 0;
				opts->fs_gid = option;
			break;
		
			case Opt_umask:
				if(match_octal(&args[0], &option))
					return 0;
				opts->fs_fmask = opts->fs_dmask = option;
			break;
		
			case Opt_dmask:
				if(match_octal(&args[0], &option))
					return 0;
				opts->fs_dmask = option;
			break;
		
			case Opt_fmask:
				if(match_octal(&args[0], &option))
					return 0;
				opts->fs_fmask = option;
			break;

			// unknown option
			default:
				printk(KERN_ERR "VXEXT: Unrecognized mount option \"%s\" "
												"or missing value\n", p);
				return 0;
		}
	}

	return 1;
}

static int vxext_calc_dir_size(struct inode *inode)
{
	struct vxext_sb_info *sbi = VXEXT_SB(inode->i_sb);
	int ret, fclus, dclus;

	inode->i_size = 0;
	if(VXEXT_I(inode)->i_start == 0)
		return 0;

	ret = vxext_get_cluster(inode, FAT_ENT_EOF, &fclus, &dclus);
	if(ret < 0)
		return ret;

	inode->i_size = (fclus + 1) * sbi->cluster_size;

	return 0;
}

static int vxext_read_root(struct inode *inode)
{
	struct super_block *sb = inode->i_sb;
	struct vxext_sb_info *sbi = VXEXT_SB(sb);

	VXEXT_I(inode)->file_cluster = VXEXT_I(inode)->disk_cluster = 0;
	VXEXT_I(inode)->i_pos = 0;
	inode->i_uid = sbi->options.fs_uid;
	inode->i_gid = sbi->options.fs_gid;
	inode->i_version++;
	inode->i_generation = 0;
	inode->i_mode = (S_IRWXUGO & ~sbi->options.fs_dmask) | S_IFDIR;
	inode->i_op = sbi->dir_ops;
	inode->i_fop = &vxext_dir_operations;
	VXEXT_I(inode)->i_start = 0;
	inode->i_size = sbi->dir_entries * sizeof(struct vxext_dir_entry);
	inode->i_blksize = sbi->cluster_size;
	inode->i_blocks = ((inode->i_size + (sbi->cluster_size - 1))
			   & ~((loff_t)sbi->cluster_size - 1)) >> 9;
	VXEXT_I(inode)->i_logstart = 0;
	VXEXT_I(inode)->mmu_private = inode->i_size;

	VXEXT_I(inode)->i_attrs = 0;
	inode->i_mtime.tv_sec = inode->i_atime.tv_sec = inode->i_ctime.tv_sec = 0;
	inode->i_mtime.tv_nsec = inode->i_atime.tv_nsec = inode->i_ctime.tv_nsec = 0;
	inode->i_nlink = vxext_subdirs(inode)+2;

	return 0;
}

/*
 * a FAT file handle with fhtype 3 is
 *  0/  i_ino - for fast, reliable lookup if still in the cache
 *  1/  i_generation - to see if i_ino is still valid
 *          bit 0 == 0 iff directory
 *  2/  i_pos(8-39) - if ino has changed, but still in cache
 *  3/  i_pos(4-7)|i_logstart - to semi-verify inode found at i_pos
 *  4/  i_pos(0-3)|parent->i_logstart - maybe used to hunt for the file on disc
 *
 * Hack for NFSv2: Maximum FAT entry number is 28bits and maximum
 * i_pos is 40bits (blocknr(32) + dir offset(8)), so two 4bits
 * of i_logstart is used to store the directory entry offset.
 */

struct dentry *vxext_decode_fh(struct super_block *sb, __u32 *fh,
															 int len, int fhtype, 
															 int (*acceptable)(void *context, struct dentry *de),
															 void *context)
{

	if(fhtype != 3)
		return ERR_PTR(-ESTALE);

	if(len < 5)
		return ERR_PTR(-ESTALE);

	return sb->s_export_op->find_exported_dentry(sb, fh, NULL, acceptable, context);
}

struct dentry *vxext_get_dentry(struct super_block *sb, void *inump)
{
	struct inode *inode = NULL;
	struct dentry *result;
	__u32 *fh = inump;

	inode = iget(sb, fh[0]);
	if(!inode || is_bad_inode(inode) || inode->i_generation != fh[1])
	{
		if(inode)
			iput(inode);

		inode = NULL;
	}

	if(!inode)
	{
		loff_t i_pos;
		int i_logstart = fh[3] & 0x0fffffff;

		i_pos = (loff_t)fh[2] << 8;
		i_pos |= ((fh[3] >> 24) & 0xf0) | (fh[4] >> 28);

		/* try 2 - see if i_pos is in F-d-c
		 * require i_logstart to be the same
		 * Will fail if you truncate and then re-write
		 */

		inode = vxext_iget(sb, i_pos);
		if(inode && VXEXT_I(inode)->i_logstart != i_logstart)
		{
			iput(inode);
			inode = NULL;
		}
	}
	
	if(!inode)
	{
		/* For now, do nothing
		 * What we could do is:
		 * follow the file starting at fh[4], and record
		 * the ".." entry, and the name of the fh[2] entry.
		 * The follow the ".." file finding the next step up.
		 * This way we build a path to the root of
		 * the tree. If this works, we lookup the path and so
		 * get this inode into the cache.
		 * Finally try the vxext_iget lookup again
		 * If that fails, then weare totally out of luck
		 * But all that is for another day
		 */
	}
	
	if(!inode)
		return ERR_PTR(-ESTALE);

	
	/* now to find a dentry.
	 * If possible, get a well-connected one
	 */
	result = d_alloc_anon(inode);
	if(result == NULL)
	{
		iput(inode);
		return ERR_PTR(-ENOMEM);
	}

	result->d_op = sb->s_root->d_op;
	return result;
}

int vxext_encode_fh(struct dentry *de, __u32 *fh, int *lenp, int connectable)
{
	int len = *lenp;
	struct inode *inode =  de->d_inode;
	u32 ipos_h, ipos_m, ipos_l;
	
	if(len < 5)
		return 255; /* no room */

	ipos_h = VXEXT_I(inode)->i_pos >> 8;
	ipos_m = (VXEXT_I(inode)->i_pos & 0xf0) << 24;
	ipos_l = (VXEXT_I(inode)->i_pos & 0x0f) << 28;
	*lenp = 5;
	fh[0] = inode->i_ino;
	fh[1] = inode->i_generation;
	fh[2] = ipos_h;
	fh[3] = ipos_m | VXEXT_I(inode)->i_logstart;
	spin_lock(&de->d_lock);
	fh[4] = ipos_l | VXEXT_I(de->d_parent->d_inode)->i_logstart;
	spin_unlock(&de->d_lock);
	
	return 3;
}

struct dentry *vxext_get_parent(struct dentry *child)
{
	struct buffer_head *bh=NULL;
	struct vxext_dir_entry *de = NULL;
	struct dentry *parent = NULL;
	int res;
	loff_t i_pos = 0;
	struct inode *inode;

	lock_kernel();
	res = vxext_scan(child->d_inode, VXEXT_DOTDOT, &bh, &de, &i_pos);
	res = -1;

	if(res < 0)
		goto out;

	inode = vxext_build_inode(child->d_sb, de, i_pos, &res);
	if(res)
		goto out;

	if(!inode)
	{
		res = -EACCES;
	}
	else
	{
		parent = d_alloc_anon(inode);
		if(!parent)
		{
			iput(inode);
			res = -ENOMEM;
		}
	}

 out:
	if(bh)
		brelse(bh);

	unlock_kernel();
	if(res)
		return ERR_PTR(res);
	else
		return parent;
}

static kmem_cache_t *vxext_inode_cachep;

static struct inode *vxext_alloc_inode(struct super_block *sb)
{
	struct vxext_inode_info *ei;

	ei = (struct vxext_inode_info *)kmem_cache_alloc(vxext_inode_cachep, SLAB_KERNEL);

	if(!ei)
		return NULL;

	return &ei->vfs_inode;
}

static void vxext_destroy_inode(struct inode *inode)
{
	kmem_cache_free(vxext_inode_cachep, VXEXT_I(inode));
}

static void vxextinit_once(void * foo, kmem_cache_t * cachep, unsigned long flags)
{
	struct vxext_inode_info *ei = (struct vxext_inode_info *) foo;

	if((flags & (SLAB_CTOR_VERIFY|SLAB_CTOR_CONSTRUCTOR)) ==
	    SLAB_CTOR_CONSTRUCTOR)
	{
		INIT_LIST_HEAD(&ei->i_fat_hash);
		inode_init_once(&ei->vfs_inode);
	}
}
 
int __init vxext_init_inodecache(void)
{
	vxext_inode_cachep = kmem_cache_create("vxext_inode_cache",
					     sizeof(struct vxext_inode_info),
					     0, SLAB_HWCACHE_ALIGN|SLAB_RECLAIM_ACCOUNT,
					     vxextinit_once, NULL);

	if(vxext_inode_cachep == NULL)
		return -ENOMEM;

	return 0;
}

void __exit vxext_destroy_inodecache(void)
{
	if(kmem_cache_destroy(vxext_inode_cachep))
		printk(KERN_INFO "vxext_inode_cache: not all structures were freed\n");
}

static int vxext_remount(struct super_block *sb, int *flags, char *data)
{
	*flags |= MS_NODIRATIME;
	return 0;
}

static struct super_operations vxext_sops =
{ 
	.alloc_inode		= vxext_alloc_inode,
	.destroy_inode	= vxext_destroy_inode,
	.write_inode		= vxext_write_inode,
	.delete_inode		= vxext_delete_inode,
	.put_super			= vxext_put_super,
	.statfs					= vxext_statfs,
	.clear_inode		= vxext_clear_inode,
	.remount_fs			= vxext_remount,
	.read_inode			= make_bad_inode,
	.show_options		= vxext_show_options,
};

static struct export_operations vxext_export_ops =
{
	.decode_fh	= vxext_decode_fh,
	.encode_fh	= vxext_encode_fh,
	.get_dentry	= vxext_get_dentry,
	.get_parent	= vxext_get_parent,
};

/*
 * Read the super block of an VxWorks extended DOS filesystem disk.
 */
int vxext_fill_super(struct super_block *sb, void *data, int silent,
										 struct inode_operations *fs_dir_inode_ops)
{
	struct inode *root_inode = NULL;
	struct buffer_head *bh;
	struct vxext_boot_sector *b;
	struct vxext_sb_info *sbi;
	u16 logical_sector_size;
	u32 total_sectors, total_clusters, vxext_clusters, rootdir_sectors;
	int debug, first;
	unsigned int media;
	long error;

	sbi = kmalloc(sizeof(struct vxext_sb_info), GFP_KERNEL);
	if(!sbi)
		return -ENOMEM;

	sb->s_fs_info = sbi;
	memset(sbi, 0, sizeof(struct vxext_sb_info));

	sb->s_flags |= MS_NODIRATIME;
	sb->s_magic = VXEXT_SUPER_MAGIC;
	sb->s_op = &vxext_sops;
	sb->s_export_op = &vxext_export_ops;
	sbi->dir_ops = fs_dir_inode_ops;

	error = -EINVAL;
	if (!parse_options(data, &debug, &sbi->options))
		goto out_fail;

	vxext_cache_init(sb);
	// set up enough so that it can read an inode
	init_MUTEX(&sbi->fat_lock);

	error = -EIO;
	sb_min_blocksize(sb, 512);
	bh = sb_bread(sb, 0);
	if (bh == NULL)
	{
		printk(KERN_ERR "VXEXT: unable to read boot sector\n");
		goto out_fail;
	}

	b = (struct vxext_boot_sector *) bh->b_data;

	#ifdef DEBUG
	printk(KERN_INFO "read bootsector:\n");
	printk(KERN_INFO "---------------\n");
	printk(KERN_INFO "system_id.........: %s\n",		b->system_id);
	printk(KERN_INFO "sector_size.......: %ld\n",		CF_LE_W(get_unaligned((u16 *)&b->sector_size)));
	printk(KERN_INFO "sec_per_clus......: %ld\n", 	b->sec_per_clus);
	printk(KERN_INFO "reserved sectors..: %ld\n",		CF_LE_W(get_unaligned((u16 *)&b->reserved)));
	printk(KERN_INFO "# of FATs.........: %ld\n",		b->fats);
	printk(KERN_INFO "max # of root dirs: %ld\n",		CF_LE_W(get_unaligned((u16 *)&b->dir_entries)));
	printk(KERN_INFO "# of sectors......: %ld\n",		CF_LE_W(get_unaligned((u16 *)&b->sectors)));
	printk(KERN_INFO "media type........: 0x%lx\n",	b->media);
	printk(KERN_INFO "# of sectors/FAT..: %d\n",		CF_LE_W(get_unaligned((u16 *)&b->fat_length)));
	printk(KERN_INFO "# of sectors/track: %d\n",		CF_LE_W(get_unaligned((u16 *)&b->secs_track)));
	printk(KERN_INFO "# of heads........: %d\n",		CF_LE_W(get_unaligned((u16 *)&b->heads)));
	printk(KERN_INFO "# of hidden sect..: %d\n",		CF_LE_L(b->hidden));
	printk(KERN_INFO "long total sectors: %ld\n",		CF_LE_L(b->total_sect));
	#endif

	// use the supermagic string for identifying the VxWorks extended DOS
	// filesystem
	if(strcmp(b->system_id, VXEXT_SUPER_MAGIC_STRING) != 0)
	{
		brelse(bh);
		goto out_fail;
	}

	if(b->reserved == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus number of reserved sectors\n");

		brelse(bh);
		goto out_invalid;
	}

	if(b->fats == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus number of FAT structure\n");

		brelse(bh);
		goto out_invalid;
	}

	if(b->secs_track == 0) 
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus sectors-per-track value\n");

		brelse(bh);
		goto out_invalid;
	}

	if(b->heads == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus number-of-heads value\n");

		brelse(bh);
		goto out_invalid;
	}
	
	media = b->media;
	if(!VXEXT_VALID_MEDIA(media))
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: invalid media value (0x%02x)\n",
			       media);

		brelse(bh);
		goto out_invalid;
	}
	
	logical_sector_size = CF_LE_W(get_unaligned((u16 *)&b->sector_size));
	if(logical_sector_size == 0
	    || (logical_sector_size & (logical_sector_size - 1))
	    || (logical_sector_size < 512)
	    || (PAGE_CACHE_SIZE < logical_sector_size))
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus logical sector size %u\n",
			       logical_sector_size);

		brelse(bh);
		goto out_invalid;
	}

	total_sectors = CF_LE_W(get_unaligned((u16 *)&b->sectors));
	if(total_sectors == 0)
		total_sectors = CF_LE_L(b->total_sect);

	if(total_sectors == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus total_sectors\n");

		brelse(bh);
		goto out_invalid;
	}

	// on VXEXT 1.0 filesystem the sectors_per_cluster field is set to
	// zero to signal that the sectors per cluster are simply
	// max_sectors / 65535
	if(b->sec_per_clus != 0)
	{
		if (!silent)
			printk(KERN_ERR "VXEXT: sec_per_cluster != 0");

		brelse(bh);
		goto out_invalid;
	}

  // calculate the sectors per cluster dynamically out of the total
  // sectors the partition has. This somehow breask the FAT16 DOS
  // specification but is exactly what Wind River does for filling
  // up the whole disk with having more than 2GB data on the disk.
  // Please note that if there is any remainder out of the division,
  // then the sectors per cluster size is increased by one, rounding
  // up to a "safe" cluster size.
  sbi->sec_per_clus = total_sectors / FAT_MAX_DIR_ENTRIES;
  if(sbi->sec_per_clus % FAT_MAX_DIR_ENTRIES)
    sbi->sec_per_clus++;

	if(sbi->sec_per_clus == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus sectors per cluster %u\n",
			       sbi->sec_per_clus);

		brelse(bh);
		goto out_invalid;
	}

	if(logical_sector_size < sb->s_blocksize)
	{
		printk(KERN_ERR "VXEXT: logical sector size too small for device"
		       " (logical sector size = %u)\n", logical_sector_size);

		brelse(bh);
		goto out_fail;
	}

	if(logical_sector_size > sb->s_blocksize)
	{
		brelse(bh);

		if(!sb_set_blocksize(sb, logical_sector_size))
		{
			printk(KERN_ERR "VXEXT: unable to set blocksize %u\n",
			       logical_sector_size);

			goto out_fail;
		}

		bh = sb_bread(sb, 0);

		if (bh == NULL)
		{
			printk(KERN_ERR "VXEXT: unable to read boot sector"
			       " (logical sector size = %lu)\n",
			       sb->s_blocksize);

			goto out_fail;
		}

		b = (struct vxext_boot_sector *) bh->b_data;
	}

	// calculate the cluster size required for accomodating
	// a cluster
	sbi->cluster_size = sb->s_blocksize * sbi->sec_per_clus;

	sbi->fats = b->fats;
	sbi->fat_start = CF_LE_W(b->reserved);
	sbi->fat_length = CF_LE_W(b->fat_length);
	sbi->root_cluster = 0;
	sbi->free_clusters = -1;	// Don't know it yet
	sbi->prev_free = -1;

	if(sbi->fat_length == 0)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus FAT length\n");

		brelse(bh);
		goto out_invalid;
	}

	sbi->dir_per_block = sb->s_blocksize / sizeof(struct vxext_dir_entry);
	sbi->dir_per_block_bits = ffs(sbi->dir_per_block) - 1;

	sbi->dir_start = sbi->fat_start + sbi->fats * sbi->fat_length;
	sbi->dir_entries = CF_LE_W(get_unaligned((u16 *)&b->dir_entries));

	if(sbi->dir_entries & (sbi->dir_per_block - 1))
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: bogus directroy-entries per block"
			       " (%u)\n", sbi->dir_entries);

		brelse(bh);
		goto out_invalid;
	}

	rootdir_sectors = sbi->dir_entries
		* sizeof(struct vxext_dir_entry) / sb->s_blocksize;
	sbi->data_start = sbi->dir_start + rootdir_sectors;

	total_clusters = (total_sectors - sbi->data_start) / sbi->sec_per_clus;

	// check that the FAT table does not overflow
	vxext_clusters = sbi->fat_length * sb->s_blocksize * 8 / 16;
	total_clusters = min(total_clusters, vxext_clusters - 2);
	if(total_clusters > MAX_FAT_VXEXT)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: count of clusters too big (%u)\n",
			       total_clusters);

		brelse(bh);
		goto out_invalid;
	}

	sbi->clusters = total_clusters;

	brelse(bh);

	// validity check of FAT
	first = __vxext_access(sb, 0, -1);
	if(first < 0)
	{
		error = first;
		goto out_fail;
	}

	if(VXEXT_FIRST_ENT(sb, media) != first)
	{
		if(!silent)
			printk(KERN_ERR "VXEXT: invalid first entry of FAT "
			       "(0x%x != 0x%x)\n",
			       VXEXT_FIRST_ENT(sb, media), first);
				   
		goto out_invalid;
	}

	error = -ENOMEM;
	root_inode = new_inode(sb);
	if(!root_inode)
		goto out_fail;

	root_inode->i_ino = VXEXT_ROOT_INO;
	root_inode->i_version = 1;
	error = vxext_read_root(root_inode);
	if(error < 0)
		goto out_fail;

	error = -ENOMEM;
	insert_inode_hash(root_inode);
	sb->s_root = d_alloc_root(root_inode);
	if(!sb->s_root)
	{
		printk(KERN_ERR "VXEXT: get root inode failed\n");
		goto out_fail;
	}

	// all is as it should be */
	printk(KERN_INFO "VFS: successfully mounted VXEXT1.0 filesystem"
									 " from device %s.\n", sb->s_id);

	return 0;

out_invalid:
	error = -EINVAL;
	if (!silent)
		printk(KERN_INFO "VFS: Can't find a valid VXEXT1.0 filesystem"
		       " on dev %s.\n", sb->s_id);

out_fail:
	if (root_inode)
		iput(root_inode);

	sb->s_fs_info = NULL;
	kfree(sbi);
	return error;
}

int vxext_statfs(struct super_block *sb, struct kstatfs *buf)
{
	int free, nr, ret;
       
	if(VXEXT_SB(sb)->free_clusters != -1)
	{
		free = VXEXT_SB(sb)->free_clusters;
	}
	else
	{
		vxlock_fat(sb);
		
		if(VXEXT_SB(sb)->free_clusters != -1)
		{
			free = VXEXT_SB(sb)->free_clusters;
		}
		else
		{
			free = 0;
			for(nr = 2; nr < VXEXT_SB(sb)->clusters + 2; nr++)
			{
				ret = vxext_access(sb, nr, -1);
				if(ret < 0)
				{
					vxunlock_fat(sb);
					return ret;
				}
				else if(ret == FAT_ENT_FREE)
				{
					free++;
				}
			}

			VXEXT_SB(sb)->free_clusters = free;
		}

		vxunlock_fat(sb);
	}

	buf->f_type = sb->s_magic;
	buf->f_bsize = VXEXT_SB(sb)->cluster_size;
	buf->f_blocks = VXEXT_SB(sb)->clusters;
	buf->f_bfree = free;
	buf->f_bavail = free;
	buf->f_namelen = VXEXT_NAMELEN; // VXEXT 1.0 support 40 chars filenames

	return 0;
}

static int vxext_writepage(struct page *page, struct writeback_control *wbc)
{
	return block_write_full_page(page,vxext_get_block, wbc);
}

static int vxext_readpage(struct file *file, struct page *page)
{
	return block_read_full_page(page,vxext_get_block);
}

static int vxext_prepare_write(struct file *file, struct page *page,
															 unsigned from, unsigned to)
{
	kmap(page);
	return cont_prepare_write(page,from,to,vxext_get_block,
		&VXEXT_I(page->mapping->host)->mmu_private);
}

static int vxext_commit_write(struct file *file, struct page *page,
			unsigned from, unsigned to)
{
	kunmap(page);
	return generic_commit_write(file, page, from, to);
}

static sector_t _vxext_bmap(struct address_space *mapping, sector_t block)
{
	return generic_block_bmap(mapping,block,vxext_get_block);
}

static struct address_space_operations vxext_aops =
{
	.readpage				= vxext_readpage,
	.writepage			= vxext_writepage,
	.sync_page			= block_sync_page,
	.prepare_write	= vxext_prepare_write,
	.commit_write		= vxext_commit_write,
	.bmap						= _vxext_bmap
};

// doesn't deal with root inode
static int vxext_fill_inode(struct inode *inode, struct vxext_dir_entry *de)
{
	struct super_block *sb = inode->i_sb;
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	int error;

	VXEXT_I(inode)->file_cluster = VXEXT_I(inode)->disk_cluster = 0;
	VXEXT_I(inode)->i_pos = 0;
	inode->i_uid = sbi->options.fs_uid;
	inode->i_gid = sbi->options.fs_gid;
	inode->i_version++;
	inode->i_generation = get_seconds();
	
	if((de->attr & ATTR_DIR) && !IS_FREE(de->name))
	{
		inode->i_generation &= ~1;
		inode->i_mode = VXEXT_MKMODE(de->attr,
			S_IRWXUGO & ~sbi->options.fs_dmask) | S_IFDIR;
		inode->i_op = sbi->dir_ops;
		inode->i_fop = &vxext_dir_operations;

		VXEXT_I(inode)->i_start = CF_LE_W(de->start);

		VXEXT_I(inode)->i_logstart = VXEXT_I(inode)->i_start;
		error = vxext_calc_dir_size(inode);

		if(error < 0)
			return error;

		VXEXT_I(inode)->mmu_private = inode->i_size;

		inode->i_nlink = vxext_subdirs(inode);
	}
	else
	{ 
		// not a directory
		inode->i_generation |= 1;
		inode->i_mode = VXEXT_MKMODE(de->attr, S_IRWXUGO
		    & ~sbi->options.fs_fmask) | S_IFREG;
		VXEXT_I(inode)->i_start = CF_LE_W(de->start);

		VXEXT_I(inode)->i_logstart = VXEXT_I(inode)->i_start;
		inode->i_size = CF_LE_L(de->size);
	        inode->i_op = &vxext_file_inode_operations;
	        inode->i_fop = &vxext_file_operations;
		inode->i_mapping->a_ops = &vxext_aops;
		VXEXT_I(inode)->mmu_private = inode->i_size;
	}

	VXEXT_I(inode)->i_attrs = de->attr & ATTR_UNUSED;
	// this is as close to the truth as we can get
	inode->i_blksize = sbi->cluster_size;
	inode->i_blocks = ((inode->i_size + (sbi->cluster_size - 1))
										 & ~((loff_t)sbi->cluster_size - 1)) >> 9;
	inode->i_mtime.tv_sec = inode->i_atime.tv_sec =
		vxext_date_dos2unix(CF_LE_W(de->time),CF_LE_W(de->date));

	inode->i_mtime.tv_nsec = inode->i_atime.tv_nsec = 0;
	inode->i_ctime.tv_sec = inode->i_mtime.tv_sec;
	inode->i_ctime.tv_nsec = 0;

	return 0;
}

void vxext_write_inode(struct inode *inode, int wait)
{
	struct super_block *sb = inode->i_sb;
	struct buffer_head *bh;
	struct vxext_dir_entry *raw_entry;
	loff_t i_pos;

retry:
	i_pos = VXEXT_I(inode)->i_pos;
	if(inode->i_ino == VXEXT_ROOT_INO || !i_pos)
	{
		return;
	}

	lock_kernel();
	if(!(bh = sb_bread(sb, i_pos >> VXEXT_SB(sb)->dir_per_block_bits)))
	{
		printk(KERN_ERR "VXEXT: unable to read inode block "
		       "for updating (i_pos %lld)\n", i_pos);
		unlock_kernel();
		return /* -EIO */;
	}

	spin_lock(&vxext_inode_lock);
	if(i_pos != VXEXT_I(inode)->i_pos)
	{
		spin_unlock(&vxext_inode_lock);
		brelse(bh);
		unlock_kernel();
		goto retry;
	}

	raw_entry = &((struct vxext_dir_entry *) (bh->b_data))
							  [i_pos & (VXEXT_SB(sb)->dir_per_block - 1)];

	if(S_ISDIR(inode->i_mode))
	{
		raw_entry->attr = ATTR_DIR;
		raw_entry->size = 0;
	}
	else
	{
		raw_entry->attr = ATTR_NONE;
		raw_entry->size = CT_LE_L(inode->i_size);
	}
	
	raw_entry->attr |= VXEXT_MKATTR(inode->i_mode) |
										 VXEXT_I(inode)->i_attrs;
	raw_entry->start = CT_LE_W(VXEXT_I(inode)->i_logstart);
	vxext_date_unix2dos(inode->i_mtime.tv_sec,&raw_entry->time,&raw_entry->date);
	raw_entry->time = CT_LE_W(raw_entry->time);
	raw_entry->date = CT_LE_W(raw_entry->date);
	spin_unlock(&vxext_inode_lock);
	mark_buffer_dirty(bh);
	brelse(bh);
	unlock_kernel();
}

int vxext_notify_change(struct dentry * dentry, struct iattr * attr)
{
	struct vxext_sb_info *sbi = VXEXT_SB(dentry->d_sb);
	struct inode *inode = dentry->d_inode;
	int mask, error = 0;

	lock_kernel();

	// FAT cannot truncate to a longer file
	if (attr->ia_valid & ATTR_SIZE)
	{
		if (attr->ia_size > inode->i_size)
		{
			error = -EPERM;
			goto out;
		}
	}

	error = inode_change_ok(inode, attr);
	if(error)
	{
		if(sbi->options.quiet)
			error = 0;
 		goto out;
	}

	if(((attr->ia_valid & ATTR_UID) && 
	    (attr->ia_uid != sbi->options.fs_uid)) ||
	  ((attr->ia_valid & ATTR_GID) && 
	    (attr->ia_gid != sbi->options.fs_gid)) ||
	   ((attr->ia_valid & ATTR_MODE) &&
	    (attr->ia_mode & ~VXEXT_VALID_MODE)))
	{
		error = -EPERM;
	}

	if(error)
	{
		if(sbi->options.quiet)  
			error = 0;
		goto out;
	}

	error = inode_setattr(inode, attr);
	if(error)
		goto out;

	if(S_ISDIR(inode->i_mode))
		mask = sbi->options.fs_dmask;
	else
		mask = sbi->options.fs_fmask;
	inode->i_mode &= S_IFMT | (S_IRWXUGO & ~mask);

out:
	unlock_kernel();
	return error;
}
