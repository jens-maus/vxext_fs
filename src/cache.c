/* vim:set ts=2 nowrap: ****************************************************

 VXEXT fs - VxWorks extended DOS filesystem support
 Copyright (c) 2004-2007 by Jens Langner <Jens.Langner@light-speed.de>

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

#include <linux/fs.h>
#include <linux/buffer_head.h>

#include "vxext_fs.h"

int __vxext_access(struct super_block *sb, int nr, int new_value)
{
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	struct buffer_head *bh, *bh2, *c_bh, *c_bh2;
	unsigned char *p_first, *p_last;
	int copy, first, last, next, b;

	first = last = nr*2;
	b = sbi->fat_start + (first >> sb->s_blocksize_bits);

	if(!(bh = sb_bread(sb, b)))
	{
		printk(KERN_ERR "VXEXT: bread(block %d) in vxext_access failed\n", b);
		
		return -EIO;
	}

	if((first >> sb->s_blocksize_bits) == (last >> sb->s_blocksize_bits))
	{
		bh2 = bh;
	}
	else
	{
		if(!(bh2 = sb_bread(sb, b + 1)))
		{
			brelse(bh);
			printk(KERN_ERR "VXEXT: bread(block %d) in vxext_access failed\n",
						 b + 1);

			return -EIO;
		}
	}

	p_first = p_last = NULL; // GCC needs that stuff
	next = CF_LE_W(((__u16 *) bh->b_data)[(first & (sb->s_blocksize - 1)) >> 1]);

	if(new_value != -1) 
	{
		((__u16 *)bh->b_data)[(first & (sb->s_blocksize - 1)) >> 1] = CT_LE_W(new_value);
		mark_buffer_dirty(bh);

		for(copy = 1; copy < sbi->fats; copy++)
		{
			b = sbi->fat_start + (first >> sb->s_blocksize_bits)
				+ sbi->fat_length * copy;

			if(!(c_bh = sb_bread(sb, b)))
				break;
			
			if(bh != bh2)
			{
				if(!(c_bh2 = sb_bread(sb, b+1)))
				{
					brelse(c_bh);
					break;
				}

				memcpy(c_bh2->b_data, bh2->b_data, sb->s_blocksize);
				mark_buffer_dirty(c_bh2);
				brelse(c_bh2);
			}

			memcpy(c_bh->b_data, bh->b_data, sb->s_blocksize);
			mark_buffer_dirty(c_bh);
			brelse(c_bh);
		}
	}

	brelse(bh);
	
	if(bh != bh2)
		brelse(bh2);
	
	return next;
}

/* 
 * Returns the this'th FAT entry, -1 if it is an end-of-file entry. If
 * new_value is != -1, that FAT entry is replaced by it.
 */
int vxext_access(struct super_block *sb, int nr, int new_value)
{
	int next;

	next = -EIO;
	if(nr < 2 || VXEXT_SB(sb)->clusters + 2 <= nr)
	{
		vxext_fs_panic(sb, "invalid access to FAT (entry 0x%08x)", nr);
		goto out;
	}

	if(new_value == FAT_ENT_EOF)
		new_value = EOF_FAT16;

	next = __vxext_access(sb, nr, new_value);
	if(next < 0)
		goto out;
	
	if(next >= BAD_FAT16)
		next = FAT_ENT_EOF;

out:
	return next;
}

void vxext_cache_init(struct super_block *sb)
{
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	int count;

	spin_lock_init(&sbi->cache_lock);

	for(count = 0; count < FAT_CACHE_NR - 1; count++)
	{
		sbi->cache_array[count].start_cluster = 0;
		sbi->cache_array[count].next = &sbi->cache_array[count + 1];
	}

	sbi->cache_array[count].start_cluster = 0;
	sbi->cache_array[count].next = NULL;
	sbi->cache = sbi->cache_array;
}

void vxext_cache_lookup(struct inode *inode, int cluster, int *f_clu, int *d_clu)
{
	struct vxext_sb_info *sbi = VXEXT_SB(inode->i_sb);
	struct vxext_cache *walk;
	int first;

	BUG_ON(cluster == 0);
	
	first = VXEXT_I(inode)->i_start;
	if(!first)
		return;

	spin_lock(&sbi->cache_lock);

	if(VXEXT_I(inode)->disk_cluster &&
	   VXEXT_I(inode)->file_cluster <= cluster)
  {
		*d_clu = VXEXT_I(inode)->disk_cluster;
		*f_clu = VXEXT_I(inode)->file_cluster;
	}

	for(walk = sbi->cache; walk; walk = walk->next)
	{
		if(walk->start_cluster == first
		    && walk->file_cluster <= cluster
		    && walk->file_cluster > *f_clu)
		{
			*d_clu = walk->disk_cluster;
			*f_clu = walk->file_cluster;

			#ifdef DEBUG
			printk("cache hit: %d (%d)\n", *f_clu, *d_clu);
			#endif
			
			if(*f_clu == cluster)
				goto out;
		}
	}

	#ifdef DEBUG
	printk("cache miss\n");
	#endif

out:
	spin_unlock(&sbi->cache_lock);
}

#ifdef DEBUG
static void list_cache(struct super_block *sb)
{
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	struct vxext_cache *walk;

	for(walk = sbi->cache; walk; walk = walk->next)
	{
		if(walk->start_cluster)
			printk("<%s,%d>(%d,%d) ", sb->s_id,
			       walk->start_cluster, walk->file_cluster,
			       walk->disk_cluster);
		else
			printk("-- ");
	}
	printk("\n");
}
#endif

/*
 * Cache invalidation occurs rarely, thus the LRU chain is not updated. It
 * fixes itself after a while.
 */
static void __vxext_cache_inval_inode(struct inode *inode)
{
	struct vxext_cache *walk;
	int first = VXEXT_I(inode)->i_start;

	VXEXT_I(inode)->file_cluster = VXEXT_I(inode)->disk_cluster = 0;
	
	for(walk = VXEXT_SB(inode->i_sb)->cache; walk; walk = walk->next)
	{
		if(walk->start_cluster == first)
		{
			walk->start_cluster = 0;
		}
	}
}

void vxext_cache_inval_inode(struct inode *inode)
{
	struct vxext_sb_info *sbi = VXEXT_SB(inode->i_sb);
	
	spin_lock(&sbi->cache_lock);
	__vxext_cache_inval_inode(inode);
	spin_unlock(&sbi->cache_lock);
}

void vxext_cache_add(struct inode *inode, int f_clu, int d_clu)
{
	struct vxext_sb_info *sbi = VXEXT_SB(inode->i_sb);
	struct vxext_cache *walk, *last;
	int first, prev_f_clu, prev_d_clu;

	if (f_clu == 0)
		return;

	first = VXEXT_I(inode)->i_start;
	if (!first)
		return;

	last = NULL;
	spin_lock(&sbi->cache_lock);

	if(VXEXT_I(inode)->file_cluster == f_clu)
	{
		goto out;
	}
	else
	{
		prev_f_clu = VXEXT_I(inode)->file_cluster;
		prev_d_clu = VXEXT_I(inode)->disk_cluster;
		VXEXT_I(inode)->file_cluster = f_clu;
		VXEXT_I(inode)->disk_cluster = d_clu;

		if(prev_f_clu == 0)
			goto out;
		
		f_clu = prev_f_clu;
		d_clu = prev_d_clu;
	}
	
	for(walk = sbi->cache; walk->next; walk = (last = walk)->next)
	{
		if(walk->start_cluster == first &&
		   walk->file_cluster == f_clu)
		{
			if(walk->disk_cluster != d_clu)
			{
				printk(KERN_ERR "VXEXT: cache corruption "
				       "(i_pos %lld)\n", VXEXT_I(inode)->i_pos);
				__vxext_cache_inval_inode(inode);

				goto out;
			}

			if(last == NULL)
				goto out;

			// update LRU
			last->next = walk->next;
			walk->next = sbi->cache;
			sbi->cache = walk;
			#ifdef DEBUG
			list_cache();
			#endif
			
			goto out;
		}
	}

	walk->start_cluster = first;
	walk->file_cluster = f_clu;
	walk->disk_cluster = d_clu;
	last->next = NULL;
	walk->next = sbi->cache;
	sbi->cache = walk;
	#ifdef DEBUG
	list_cache();
	#endif
out:
	spin_unlock(&sbi->cache_lock);
}

int vxext_get_cluster(struct inode *inode, int cluster, int *fclus, int *dclus)
{
	struct super_block *sb = inode->i_sb;
	int limit = ((unsigned long)sb->s_maxbytes) / VXEXT_SB(sb)->cluster_size;
	int nr;

	// vxworks uses cluster sizes not bound to base 2 so lets see if we
	// need to add a cluster or not.
	if(((unsigned long)sb->s_maxbytes) % VXEXT_SB(sb)->cluster_size)
		limit++;
	
	BUG_ON(VXEXT_I(inode)->i_start == 0);
	
	*fclus = 0;
	*dclus = VXEXT_I(inode)->i_start;
	if (cluster == 0)
		return 0;

	vxext_cache_lookup(inode, cluster, fclus, dclus);
	while(*fclus < cluster)
	{
		// prevent the infinite loop of cluster chain
		if(*fclus > limit)
		{
			vxext_fs_panic(sb, "%s: detected the cluster chain loop"
				     " (i_pos %lld)", __FUNCTION__,
				     VXEXT_I(inode)->i_pos);
			return -EIO;
		}

		nr = vxext_access(sb, *dclus, -1);
		if(nr < 0)
		{
 			return nr;
		}
		else if(nr == FAT_ENT_FREE)
		{
			vxext_fs_panic(sb, "%s: invalid cluster chain"
				     " (i_pos %lld)", __FUNCTION__,
				     VXEXT_I(inode)->i_pos);
			return -EIO;
		}
		else if(nr == FAT_ENT_EOF)
		{
			vxext_cache_add(inode, *fclus, *dclus);
			return FAT_ENT_EOF;
		}

		(*fclus)++;
		*dclus = nr;
	}

	vxext_cache_add(inode, *fclus, *dclus);
	return 0;
}

static int vxext_bmap_cluster(struct inode *inode, int cluster)
{
	struct super_block *sb = inode->i_sb;
	int ret, fclus, dclus;

	if(VXEXT_I(inode)->i_start == 0)
		return 0;

	ret = vxext_get_cluster(inode, cluster, &fclus, &dclus);

	if(ret < 0)
	{
		return ret;
	}
	else if(ret == FAT_ENT_EOF)
	{
		vxext_fs_panic(sb, "%s: request beyond EOF (i_pos %lld %ld)",
			     __FUNCTION__, VXEXT_I(inode)->i_pos, cluster);

		return -EIO;
	}
	
	return dclus;
}

int vxext_bmap(struct inode *inode, sector_t sector, sector_t *phys)
{
	struct super_block *sb = inode->i_sb;
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	sector_t last_block;
	int cluster, offset;

	*phys = 0;
	if((inode->i_ino == VXEXT_ROOT_INO || (S_ISDIR(inode->i_mode) &&
	    !VXEXT_I(inode)->i_start)))
	{
		if (sector < (sbi->dir_entries >> sbi->dir_per_block_bits))
			*phys = sector + sbi->dir_start;

		return 0;
	}

	last_block = (VXEXT_I(inode)->mmu_private + (sb->s_blocksize - 1))
		>> sb->s_blocksize_bits;

	if(sector >= last_block)
		return 0;

	cluster = ((unsigned long)sector) / sbi->sec_per_clus; 
	offset  = ((unsigned long)sector) % sbi->sec_per_clus;
	cluster = vxext_bmap_cluster(inode, cluster);

	if(cluster < 0)
	{
		return cluster;
	}
	else if (cluster) 
	{
		*phys = ((sector_t)cluster - 2) * sbi->sec_per_clus
			+ sbi->data_start + offset;
	}

	return 0;
}

// Free all clusters after the skip'th cluster.
int vxext_free(struct inode *inode, int skip)
{
	struct super_block *sb = inode->i_sb;
	int nr, ret, fclus, dclus;

	if(VXEXT_I(inode)->i_start == 0)
		return 0;

	if(skip)
	{
		ret = vxext_get_cluster(inode, skip - 1, &fclus, &dclus);
		if (ret < 0)
			return ret;
		else if (ret == FAT_ENT_EOF)
			return 0;

		nr = vxext_access(sb, dclus, -1);
		if (nr == FAT_ENT_EOF)
		{
			return 0;
		}
		else if (nr > 0)
		{
			// write a new EOF, and get the remaining cluster
			// chain for freeing. 
			nr = vxext_access(sb, dclus, FAT_ENT_EOF);
		}

		if(nr < 0)
			return nr;

		vxext_cache_inval_inode(inode);
	}
	else
	{
		vxext_cache_inval_inode(inode);

		nr = VXEXT_I(inode)->i_start;
		VXEXT_I(inode)->i_start = 0;
		VXEXT_I(inode)->i_logstart = 0;
		mark_inode_dirty(inode);
	}

	vxlock_fat(sb);
	do
	{
		nr = vxext_access(sb, nr, FAT_ENT_FREE);
		if(nr < 0)
		{
			goto error;
		}
		else if(nr == FAT_ENT_FREE)
		{
			vxext_fs_panic(sb, "%s: deleting beyond EOF (i_pos %lld)",
				     __FUNCTION__, VXEXT_I(inode)->i_pos);
			nr = -EIO;
			goto error;
		}

		if(VXEXT_SB(sb)->free_clusters != -1)
			VXEXT_SB(sb)->free_clusters++;
		inode->i_blocks -= VXEXT_SB(sb)->cluster_size >> 9;
	}
	while (nr != FAT_ENT_EOF);

	nr = 0;
error:
	vxunlock_fat(sb);

	return nr;
}
