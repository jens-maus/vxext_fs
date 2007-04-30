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
#include <linux/buffer_head.h>
#include <linux/smp_lock.h>

#include "vxext_fs.h"

// normally this function is used to analyze filenames and check
// if they conform. But as there are no real conventions for filenames
// on VxWorks, we just copy&paste the filename to the resulting res
// string.
static int vxext_format_name(const unsigned char *name, int len,
														 unsigned char *res,
														 struct vxext_mount_options *opts)
{
	memcpy(res, name, len*sizeof(char));
	while(len < VXEXT_NAMELEN) 
	{
		res[len] = ' ';
		len++;
	}

	return 0;
}

// Locates a directory entry.  Uses unformatted name.
static int vxext_find(struct inode *dir, const unsigned char *name, int len,
											struct buffer_head **bh, struct vxext_dir_entry **de,
											loff_t *i_pos)
{
	unsigned char vxext_name[VXEXT_NAMELEN];
	int res;

	res = vxext_format_name(name,len, vxext_name, &VXEXT_SB(dir->i_sb)->options);
	if(res < 0)
		return -ENOENT;

	res = vxext_scan(dir, vxext_name, bh, de, i_pos);
	if(!res)
	{
		if(name[0]=='.')
		{
			if(!((*de)->attr & ATTR_HIDDEN))
				res = -ENOENT;
		}
		else
		{
			if((*de)->attr & ATTR_HIDDEN)
				res = -ENOENT;
		}
	}

	return res;
}

/*
 * Compute the hash for the msdos name corresponding to the dentry.
 * Note: if the name is invalid, we leave the hash code unchanged so
 * that the existing dentry can be used. The msdos fs routines will
 * return ENOENT or EINVAL as appropriate.
 */
static int vxext_hash(struct dentry *dentry, struct qstr *qstr)
{
	struct vxext_mount_options *options = &(VXEXT_SB(dentry->d_sb)->options);
	unsigned char vxext_name[VXEXT_NAMELEN];
	int error;
	
	error = vxext_format_name(qstr->name, qstr->len, vxext_name, options);
	if(!error)
		qstr->hash = full_name_hash(vxext_name, VXEXT_NAMELEN);

	return 0;
}

/*
 * Compare two msdos names. If either of the names are invalid,
 * we fall back to doing the standard name comparison.
 */
static int vxext_cmp(struct dentry *dentry, struct qstr *a, struct qstr *b)
{
	struct vxext_mount_options *options = & (VXEXT_SB(dentry->d_sb)->options);
	unsigned char a_vxext_name[VXEXT_NAMELEN], b_vxext_name[VXEXT_NAMELEN];
	int error;

	error = vxext_format_name(a->name, a->len, a_vxext_name, options);
	if(error)
		goto old_compare;

	error = vxext_format_name(b->name, b->len, b_vxext_name, options);
	if(error)
		goto old_compare;

	error = memcmp(a_vxext_name, b_vxext_name, VXEXT_NAMELEN);

out:
	return error;

old_compare:
	error = 1;
	if(a->len == b->len)
		error = memcmp(a->name, b->name, a->len);
	goto out;
}


static struct dentry_operations vxext_dentry_operations =
{
	.d_hash			= vxext_hash,
	.d_compare	= vxext_cmp,
};

/*
 * AV. Wrappers for FAT sb operations. Is it wise?
 */

// Get inode using directory and name
static struct dentry *vxext_lookup(struct inode *dir, struct dentry *dentry,
																	 struct nameidata *nd)
{
	struct super_block *sb = dir->i_sb;
	struct inode *inode = NULL;
	struct vxext_dir_entry *de;
	struct buffer_head *bh = NULL;
	loff_t i_pos;
	int res;
	
	dentry->d_op = &vxext_dentry_operations;

	lock_kernel();
	res = vxext_find(dir, dentry->d_name.name, dentry->d_name.len, &bh,
			 &de, &i_pos);

	if(res == -ENOENT)
		goto add;

	if(res < 0)
		goto out;

	inode = vxext_build_inode(sb, de, i_pos, &res);
	if(res)
		goto out;

add:
	res = 0;
	dentry = d_splice_alias(inode, dentry);
	if(dentry)
		dentry->d_op = &vxext_dentry_operations;

out:
	brelse(bh);
	unlock_kernel();
	if(!res)
		return dentry;

	return ERR_PTR(res);
}

// Creates a directory entry (name is already formatted).
static int vxext_add_entry(struct inode *dir, const unsigned char *name,
													 struct buffer_head **bh,
													 struct vxext_dir_entry **de,
													 loff_t *i_pos, int is_dir, int is_hid)
{
	int res;

	res = vxext_add_entries(dir, 1, bh, de, i_pos);
	if(res < 0)
		return res;

	/*
	 * XXX all times should be set by caller upon successful completion.
	 */
	dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	mark_inode_dirty(dir);

	memcpy((*de)->name, name, VXEXT_NAMELEN);
	(*de)->attr = is_dir ? ATTR_DIR : ATTR_ARCH;
	if(is_hid)
		(*de)->attr |= ATTR_HIDDEN;

	(*de)->start = 0;
	vxext_date_unix2dos(dir->i_mtime.tv_sec, &(*de)->time, &(*de)->date);
	(*de)->size = 0;
	mark_buffer_dirty(*bh);

	return 0;
}

/***** Create a file */
static int vxext_create(struct inode *dir, struct dentry *dentry, int mode,
												struct nameidata *nd)
{
	struct super_block *sb = dir->i_sb;
	struct buffer_head *bh;
	struct vxext_dir_entry *de;
	struct inode *inode;
	loff_t i_pos;
	int res, is_hid;
	unsigned char vxext_name[VXEXT_NAMELEN];

	lock_kernel();
	res = vxext_format_name(dentry->d_name.name,dentry->d_name.len,
				vxext_name, &VXEXT_SB(sb)->options);

	if(res < 0)
	{
		unlock_kernel();
		return res;
	}

	is_hid = (dentry->d_name.name[0]=='.') && (vxext_name[0]!='.');

	// Have to do it due to foo vs. .foo conflicts
	if(vxext_scan(dir, vxext_name, &bh, &de, &i_pos) >= 0)
	{
		brelse(bh);
		unlock_kernel();
		return -EINVAL;
 	}

	inode = NULL;
	res = vxext_add_entry(dir, vxext_name, &bh, &de, &i_pos, 0, is_hid);
	if(res)
	{
		unlock_kernel();
		return res;
	}

	inode = vxext_build_inode(dir->i_sb, de, i_pos, &res);
	brelse(bh);
	if(!inode)
	{
		unlock_kernel();
		return res;
	}

	inode->i_mtime = inode->i_atime = inode->i_ctime = CURRENT_TIME;
	mark_inode_dirty(inode);
	d_instantiate(dentry, inode);
	unlock_kernel();

	return 0;
}

// Remove a directory
static int vxext_rmdir(struct inode *dir, struct dentry *dentry)
{
	struct inode *inode = dentry->d_inode;
	loff_t i_pos;
	int res;
	struct buffer_head *bh;
	struct vxext_dir_entry *de;

	bh = NULL;
	lock_kernel();
	res = vxext_find(dir, dentry->d_name.name, dentry->d_name.len,
			 &bh, &de, &i_pos);

	if(res < 0)
		goto rmdir_done;

	/*
	 * Check whether the directory is not in use, then check
	 * whether it is empty.
	 */
	res = vxext_dir_empty(inode);
	if(res)
		goto rmdir_done;

	de->name[0] = DELETED_FLAG;
	mark_buffer_dirty(bh);
	vxext_detach(inode);
	inode->i_nlink = 0;
	inode->i_ctime = dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	dir->i_nlink--;
	mark_inode_dirty(inode);
	mark_inode_dirty(dir);
	res = 0;

rmdir_done:
	brelse(bh);
	unlock_kernel();
	return res;
}

// Make a directory
static int vxext_mkdir(struct inode *dir, struct dentry *dentry, int mode)
{
	struct super_block *sb = dir->i_sb;
	struct buffer_head *bh;
	struct vxext_dir_entry *de;
	struct inode *inode;
	int res,is_hid;
	unsigned char vxext_name[VXEXT_NAMELEN];
	loff_t i_pos;

	lock_kernel();
	res = vxext_format_name(dentry->d_name.name,dentry->d_name.len,
				vxext_name, &VXEXT_SB(sb)->options);
	
	if(res < 0)
	{
		unlock_kernel();
		return res;
	}

	is_hid = (dentry->d_name.name[0]=='.') && (vxext_name[0]!='.');
	// foo vs .foo situation
	if(vxext_scan(dir, vxext_name, &bh, &de, &i_pos) >= 0)
		goto out_exist;

	res = vxext_add_entry(dir, vxext_name, &bh, &de, &i_pos, 1, is_hid);
	if(res)
		goto out_unlock;

	inode = vxext_build_inode(dir->i_sb, de, i_pos, &res);
	if(!inode)
	{
		brelse(bh);
		goto out_unlock;
	}

	res = 0;

	dir->i_nlink++;
	inode->i_nlink = 2; // no need to mark them dirty

	res = vxext_new_dir(inode, dir);
	if(res)
		goto mkdir_error;

	brelse(bh);
	d_instantiate(dentry, inode);
	res = 0;

out_unlock:
	unlock_kernel();
	return res;

mkdir_error:
	inode->i_nlink = 0;
	inode->i_ctime = dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	dir->i_nlink--;
	mark_inode_dirty(inode);
	mark_inode_dirty(dir);
	de->name[0] = DELETED_FLAG;
	mark_buffer_dirty(bh);
	brelse(bh);
	vxext_detach(inode);
	iput(inode);
	goto out_unlock;

out_exist:
	brelse(bh);
	res = -EINVAL;
	goto out_unlock;
}

// Unlink a file */
static int vxext_unlink(struct inode *dir, struct dentry *dentry)
{
	struct inode *inode = dentry->d_inode;
	loff_t i_pos;
	int res;
	struct buffer_head *bh;
	struct vxext_dir_entry *de;

	bh = NULL;
	lock_kernel();
	res = vxext_find(dir, dentry->d_name.name, dentry->d_name.len,
			 &bh, &de, &i_pos);

	if(res < 0)
		goto unlink_done;

	de->name[0] = DELETED_FLAG;
	mark_buffer_dirty(bh);
	vxext_detach(inode);
	brelse(bh);
	inode->i_nlink = 0;
	inode->i_ctime = dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	mark_inode_dirty(inode);
	mark_inode_dirty(dir);
	res = 0;

unlink_done:
	unlock_kernel();
	return res;
}

static int do_vxext_rename(struct inode *old_dir, unsigned char *old_name,
													 struct dentry *old_dentry,
													 struct inode *new_dir, unsigned char *new_name,
													 struct dentry *new_dentry,
													 struct buffer_head *old_bh,
													 struct vxext_dir_entry *old_de,
													 loff_t old_i_pos, int is_hid)
{
	struct buffer_head *new_bh=NULL,*dotdot_bh=NULL;
	struct vxext_dir_entry *new_de,*dotdot_de;
	struct inode *old_inode,*new_inode;
	loff_t new_i_pos, dotdot_i_pos;
	int error;
	int is_dir;

	old_inode = old_dentry->d_inode;
	new_inode = new_dentry->d_inode;
	is_dir = S_ISDIR(old_inode->i_mode);

	if(vxext_scan(new_dir, new_name, &new_bh, &new_de, &new_i_pos) >= 0
	   && !new_inode)
	{
		goto degenerate_case;
	}

	if(is_dir)
	{
		if(new_inode)
		{
			error = vxext_dir_empty(new_inode);
			if (error)
				goto out;
		}

		if(vxext_scan(old_inode, VXEXT_DOTDOT, &dotdot_bh,
			     &dotdot_de, &dotdot_i_pos) < 0)
		{
			error = -EIO;
			goto out;
		}
	}

	if(!new_bh)
	{
		error = vxext_add_entry(new_dir, new_name, &new_bh, &new_de,
						&new_i_pos, is_dir, is_hid);
		if(error)
			goto out;
	}

	new_dir->i_version++;

	// There we go
	if(new_inode)
		vxext_detach(new_inode);
		
	old_de->name[0] = DELETED_FLAG;
	mark_buffer_dirty(old_bh);
	vxext_detach(old_inode);
	vxext_attach(old_inode, new_i_pos);
	
	if(is_hid)
		VXEXT_I(old_inode)->i_attrs |= ATTR_HIDDEN;
	else
		VXEXT_I(old_inode)->i_attrs &= ~ATTR_HIDDEN;

	mark_inode_dirty(old_inode);
	old_dir->i_version++;
	old_dir->i_ctime = old_dir->i_mtime = CURRENT_TIME;
	mark_inode_dirty(old_dir);

	if(new_inode)
	{
		new_inode->i_nlink--;
		new_inode->i_ctime = CURRENT_TIME;
		mark_inode_dirty(new_inode);
	}

	if(dotdot_bh)
	{
		dotdot_de->start = CT_LE_W(VXEXT_I(new_dir)->i_logstart);
		mark_buffer_dirty(dotdot_bh);
		old_dir->i_nlink--;
		mark_inode_dirty(old_dir);

		if(new_inode)
		{
			new_inode->i_nlink--;
			mark_inode_dirty(new_inode);
		}
		else
		{
			new_dir->i_nlink++;
			mark_inode_dirty(new_dir);
		}
	}
	error = 0;

out:
	brelse(new_bh);
	brelse(dotdot_bh);
	return error;

degenerate_case:
	error = -EINVAL;

	if(new_de!=old_de)
		goto out;
	if(is_hid)
		VXEXT_I(old_inode)->i_attrs |= ATTR_HIDDEN;
	else
		VXEXT_I(old_inode)->i_attrs &= ~ATTR_HIDDEN;

	mark_inode_dirty(old_inode);
	old_dir->i_version++;
	old_dir->i_ctime = old_dir->i_mtime = CURRENT_TIME;
	mark_inode_dirty(old_dir);

	return 0;
}

// Rename, a wrapper for rename_same_dir & rename_diff_dir
static int vxext_rename(struct inode *old_dir, struct dentry *old_dentry,
												struct inode *new_dir, struct dentry *new_dentry)
{
	struct buffer_head *old_bh;
	struct vxext_dir_entry *old_de;
	loff_t old_i_pos;
	int error, is_hid, old_hid; // if new file and old file are hidden
	unsigned char old_vxext_name[VXEXT_NAMELEN], new_vxext_name[VXEXT_NAMELEN];

	lock_kernel();
	error = vxext_format_name(old_dentry->d_name.name,
				  old_dentry->d_name.len,old_vxext_name,
				  &VXEXT_SB(old_dir->i_sb)->options);
	if(error < 0)
		goto rename_done;

	error = vxext_format_name(new_dentry->d_name.name,
				  new_dentry->d_name.len,new_vxext_name,
				  &VXEXT_SB(new_dir->i_sb)->options);
	if(error < 0)
		goto rename_done;

	is_hid  = (new_dentry->d_name.name[0]=='.') && (new_vxext_name[0]!='.');
	old_hid = (old_dentry->d_name.name[0]=='.') && (old_vxext_name[0]!='.');
	error = vxext_scan(old_dir, old_vxext_name, &old_bh, &old_de, &old_i_pos);
	if(error < 0)
		goto rename_done;

	error = do_vxext_rename(old_dir, old_vxext_name, old_dentry,
				new_dir, new_vxext_name, new_dentry,
				old_bh, old_de, old_i_pos, is_hid);
	brelse(old_bh);

rename_done:
	unlock_kernel();
	return error;
}

static struct inode_operations vxext_dir_inode_operations =
{
	.lookup		= vxext_lookup,
	.create		= vxext_create,
	.unlink		= vxext_unlink,
	.mkdir		= vxext_mkdir,
	.rmdir		= vxext_rmdir,
	.rename		= vxext_rename,
	.setattr	= vxext_notify_change,
};

static int vx_fill_super(struct super_block *sb,void *data, int silent)
{
	int res;

	res = vxext_fill_super(sb, data, silent, &vxext_dir_inode_operations);
	if(res)
	  return res;

	sb->s_root->d_op = &vxext_dentry_operations;
	return 0;
}

static int vxext_get_sb(struct file_system_type *fs_type,
												int flags, const char *dev_name,
												void *data, struct vfsmount *mnt)
{
	return get_sb_bdev(fs_type, flags, dev_name, data, vx_fill_super, mnt);
}

static struct file_system_type vxext_fs_type =
{
	.owner		= THIS_MODULE,
	.name			= "vxext",
	.get_sb		= vxext_get_sb,
	.kill_sb	= kill_block_super,
	.fs_flags	= FS_REQUIRES_DEV,
};

static int __init init_vxext_fs(void)
{
	vxext_hash_init();
	if(vxext_init_inodecache() == 0)
	{
		return register_filesystem(&vxext_fs_type);
	}

	return -ENOMEM;
}

static void __exit exit_vxext_fs(void)
{
	vxext_destroy_inodecache();
	unregister_filesystem(&vxext_fs_type);
}

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Jens Langner");
MODULE_DESCRIPTION("VxWorks extended DOS filesystem support");
MODULE_VERSION("1.1");

module_init(init_vxext_fs)
module_exit(exit_vxext_fs)
