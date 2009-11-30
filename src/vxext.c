/* *************************************************************************

 VXEXT fs - VxWorks extended DOS filesystem support
 Copyright (c) 2004-2009 by Jens Langner <Jens.Langner@light-speed.de>

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

 $Id: Makefile 21 2007-04-30 14:45:02Z langner $

***************************************************************************/

/*
 *  linux/fs/msdos/namei.c
 *
 *  Written 1992,1993 by Werner Almesberger
 *  Hidden files 1995 by Albert Cahalan <albert@ccs.neu.edu> <adc@coe.neu.edu>
 *  Rewritten for constant inumbers 1999 by Al Viro
 */

#include <linux/module.h>
#include <linux/time.h>
#include <linux/buffer_head.h>
#include "fat.h"

// normally this function is used to analyze filenames and check
// if they conform. But as there are no real conventions for filenames
// on VxWorks, we just copy&paste the filename to the resulting res
// string and fill it with white spaces
static int vxext_format_name(const unsigned char *name, int len,
			     unsigned char *res, struct fat_mount_options *opts)
{
  memcpy(res, name, len*sizeof(char));
  while(len < MSDOS_NAME)
  {
    res[len] = ' ';
    len++;
  }

  return 0;
}

/***** Locates a directory entry.  Uses unformatted name. */
static int vxext_find(struct inode *dir, const unsigned char *name, int len,
		      struct fat_slot_info *sinfo)
{
	struct msdos_sb_info *sbi = MSDOS_SB(dir->i_sb);
	unsigned char msdos_name[MSDOS_NAME];
	int err;

	err = vxext_format_name(name, len, msdos_name, &sbi->options);
	if (err)
		return -ENOENT;

	err = fat_scan(dir, msdos_name, sinfo);
	if (!err && sbi->options.dotsOK) {
		if (name[0] == '.') {
			if (!(sinfo->de->attr & ATTR_HIDDEN))
				err = -ENOENT;
		} else {
			if (sinfo->de->attr & ATTR_HIDDEN)
				err = -ENOENT;
		}
		if (err)
			brelse(sinfo->bh);
	}
	return err;
}

/*
 * Compute the hash for the msdos name corresponding to the dentry.
 * Note: if the name is invalid, we leave the hash code unchanged so
 * that the existing dentry can be used. The msdos fs routines will
 * return ENOENT or EINVAL as appropriate.
 */
static int vxext_hash(struct dentry *dentry, struct qstr *qstr)
{
	struct fat_mount_options *options = &MSDOS_SB(dentry->d_sb)->options;
	unsigned char msdos_name[MSDOS_NAME];
	int error;

	error = vxext_format_name(qstr->name, qstr->len, msdos_name, options);
	if (!error)
		qstr->hash = full_name_hash(msdos_name, MSDOS_NAME);
	return 0;
}

/*
 * Compare two msdos names. If either of the names are invalid,
 * we fall back to doing the standard name comparison.
 */
static int vxext_cmp(struct dentry *dentry, struct qstr *a, struct qstr *b)
{
	struct fat_mount_options *options = &MSDOS_SB(dentry->d_sb)->options;
	unsigned char a_msdos_name[MSDOS_NAME], b_msdos_name[MSDOS_NAME];
	int error;

	error = vxext_format_name(a->name, a->len, a_msdos_name, options);
	if (error)
		goto old_compare;
	error = vxext_format_name(b->name, b->len, b_msdos_name, options);
	if (error)
		goto old_compare;
	error = memcmp(a_msdos_name, b_msdos_name, MSDOS_NAME);
out:
	return error;

old_compare:
	error = 1;
	if (a->len == b->len)
		error = memcmp(a->name, b->name, a->len);
	goto out;
}

static const struct dentry_operations vxext_dentry_operations = {
	.d_hash		= vxext_hash,
	.d_compare	= vxext_cmp,
};

/*
 * AV. Wrappers for FAT sb operations. Is it wise?
 */

/***** Get inode using directory and name */
static struct dentry *vxext_lookup(struct inode *dir, struct dentry *dentry,
				   struct nameidata *nd)
{
	struct super_block *sb = dir->i_sb;
	struct fat_slot_info sinfo;
	struct inode *inode;
	int err;

	lock_super(sb);
	err = vxext_find(dir, dentry->d_name.name, dentry->d_name.len, &sinfo);
	if (err) {
		if (err == -ENOENT) {
			inode = NULL;
			goto out;
		}
		goto error;
	}

	inode = fat_build_inode(sb, sinfo.de, sinfo.i_pos);
	brelse(sinfo.bh);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		goto error;
	}
out:
	unlock_super(sb);
	dentry->d_op = &vxext_dentry_operations;
	dentry = d_splice_alias(inode, dentry);
	if (dentry)
		dentry->d_op = &vxext_dentry_operations;
	return dentry;

error:
	unlock_super(sb);
	return ERR_PTR(err);
}

/***** Creates a directory entry (name is already formatted). */
static int vxext_add_entry(struct inode *dir, const unsigned char *name,
			   int is_dir, int is_hid, int cluster,
			   struct timespec *ts, struct fat_slot_info *sinfo)
{
	struct msdos_sb_info *sbi = MSDOS_SB(dir->i_sb);
	struct msdos_dir_entry de;
	__le16 time, date;
	int err;

	memcpy(de.name, name, MSDOS_NAME);
	de.attr = is_dir ? ATTR_DIR : ATTR_ARCH;
	if (is_hid)
		de.attr |= ATTR_HIDDEN;
   #ifndef VXEXT_FS
	de.lcase = 0;
   #endif
	fat_time_unix2fat(sbi, ts, &time, &date, NULL);
   #ifndef VXEXT_FS
	de.cdate = de.adate = 0;
	de.ctime = 0;
	de.ctime_cs = 0;
   #endif
	de.time = time;
	de.date = date;
	de.start = cpu_to_le16(cluster);
   #ifndef VXEXT_FS
	de.starthi = cpu_to_le16(cluster >> 16);
   #endif
	de.size = 0;

	err = fat_add_entries(dir, &de, 1, sinfo);
	if (err)
		return err;

	dir->i_ctime = dir->i_mtime = *ts;
	if (IS_DIRSYNC(dir))
		(void)fat_sync_inode(dir);
	else
		mark_inode_dirty(dir);

	return 0;
}

/***** Create a file */
static int vxext_create(struct inode *dir, struct dentry *dentry, int mode,
			struct nameidata *nd)
{
	struct super_block *sb = dir->i_sb;
	struct inode *inode = NULL;
	struct fat_slot_info sinfo;
	struct timespec ts;
	unsigned char msdos_name[MSDOS_NAME];
	int err, is_hid;

	lock_super(sb);

	err = vxext_format_name(dentry->d_name.name, dentry->d_name.len,
				msdos_name, &MSDOS_SB(sb)->options);
	if (err)
		goto out;
	is_hid = (dentry->d_name.name[0] == '.') && (msdos_name[0] != '.');
	/* Have to do it due to foo vs. .foo conflicts */
	if (!fat_scan(dir, msdos_name, &sinfo)) {
		brelse(sinfo.bh);
		err = -EINVAL;
		goto out;
	}

	ts = CURRENT_TIME_SEC;
	err = vxext_add_entry(dir, msdos_name, 0, is_hid, 0, &ts, &sinfo);
	if (err)
		goto out;
	inode = fat_build_inode(sb, sinfo.de, sinfo.i_pos);
	brelse(sinfo.bh);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		goto out;
	}
	inode->i_mtime = inode->i_atime = inode->i_ctime = ts;
	/* timestamp is already written, so mark_inode_dirty() is unneeded. */

	d_instantiate(dentry, inode);
out:
	unlock_super(sb);
	if (!err)
		err = fat_flush_inodes(sb, dir, inode);
	return err;
}

/***** Remove a directory */
static int vxext_rmdir(struct inode *dir, struct dentry *dentry)
{
	struct super_block *sb = dir->i_sb;
	struct inode *inode = dentry->d_inode;
	struct fat_slot_info sinfo;
	int err;

	lock_super(sb);
	/*
	 * Check whether the directory is not in use, then check
	 * whether it is empty.
	 */
	err = fat_dir_empty(inode);
	if (err)
		goto out;
	err = vxext_find(dir, dentry->d_name.name, dentry->d_name.len, &sinfo);
	if (err)
		goto out;

	err = fat_remove_entries(dir, &sinfo);	/* and releases bh */
	if (err)
		goto out;
	drop_nlink(dir);

	clear_nlink(inode);
	inode->i_ctime = CURRENT_TIME_SEC;
	fat_detach(inode);
out:
	unlock_super(sb);
	if (!err)
		err = fat_flush_inodes(sb, dir, inode);

	return err;
}

/***** Make a directory */
static int vxext_mkdir(struct inode *dir, struct dentry *dentry, int mode)
{
	struct super_block *sb = dir->i_sb;
	struct fat_slot_info sinfo;
	struct inode *inode;
	unsigned char msdos_name[MSDOS_NAME];
	struct timespec ts;
	int err, is_hid, cluster;

	lock_super(sb);

	err = vxext_format_name(dentry->d_name.name, dentry->d_name.len,
				msdos_name, &MSDOS_SB(sb)->options);
	if (err)
		goto out;
	is_hid = (dentry->d_name.name[0] == '.') && (msdos_name[0] != '.');
	/* foo vs .foo situation */
	if (!fat_scan(dir, msdos_name, &sinfo)) {
		brelse(sinfo.bh);
		err = -EINVAL;
		goto out;
	}

	ts = CURRENT_TIME_SEC;
	cluster = fat_alloc_new_dir(dir, &ts);
	if (cluster < 0) {
		err = cluster;
		goto out;
	}
	err = vxext_add_entry(dir, msdos_name, 1, is_hid, cluster, &ts, &sinfo);
	if (err)
		goto out_free;
	inc_nlink(dir);

	inode = fat_build_inode(sb, sinfo.de, sinfo.i_pos);
	brelse(sinfo.bh);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		/* the directory was completed, just return a error */
		goto out;
	}
	inode->i_nlink = 2;
	inode->i_mtime = inode->i_atime = inode->i_ctime = ts;
	/* timestamp is already written, so mark_inode_dirty() is unneeded. */

	d_instantiate(dentry, inode);

	unlock_super(sb);
	fat_flush_inodes(sb, dir, inode);
	return 0;

out_free:
	fat_free_clusters(dir, cluster);
out:
	unlock_super(sb);
	return err;
}

/***** Unlink a file */
static int vxext_unlink(struct inode *dir, struct dentry *dentry)
{
	struct inode *inode = dentry->d_inode;
	struct super_block *sb= inode->i_sb;
	struct fat_slot_info sinfo;
	int err;

	lock_super(sb);
	err = vxext_find(dir, dentry->d_name.name, dentry->d_name.len, &sinfo);
	if (err)
		goto out;

	err = fat_remove_entries(dir, &sinfo);	/* and releases bh */
	if (err)
		goto out;
	clear_nlink(inode);
	inode->i_ctime = CURRENT_TIME_SEC;
	fat_detach(inode);
out:
	unlock_super(sb);
	if (!err)
		err = fat_flush_inodes(sb, dir, inode);

	return err;
}

static int do_vxext_rename(struct inode *old_dir, unsigned char *old_name,
			   struct dentry *old_dentry,
			   struct inode *new_dir, unsigned char *new_name,
			   struct dentry *new_dentry, int is_hid)
{
	struct buffer_head *dotdot_bh;
	struct msdos_dir_entry *dotdot_de;
	struct inode *old_inode, *new_inode;
	struct fat_slot_info old_sinfo, sinfo;
	struct timespec ts;
	loff_t dotdot_i_pos, new_i_pos;
	int err, old_attrs, is_dir, update_dotdot, corrupt = 0;

	old_sinfo.bh = sinfo.bh = dotdot_bh = NULL;
	old_inode = old_dentry->d_inode;
	new_inode = new_dentry->d_inode;

	err = fat_scan(old_dir, old_name, &old_sinfo);
	if (err) {
		err = -EIO;
		goto out;
	}

	is_dir = S_ISDIR(old_inode->i_mode);
	update_dotdot = (is_dir && old_dir != new_dir);
	if (update_dotdot) {
		if (fat_get_dotdot_entry(old_inode, &dotdot_bh, &dotdot_de,
					 &dotdot_i_pos) < 0) {
			err = -EIO;
			goto out;
		}
	}

	old_attrs = MSDOS_I(old_inode)->i_attrs;
	err = fat_scan(new_dir, new_name, &sinfo);
	if (!err) {
		if (!new_inode) {
			/* "foo" -> ".foo" case. just change the ATTR_HIDDEN */
			if (sinfo.de != old_sinfo.de) {
				err = -EINVAL;
				goto out;
			}
			if (is_hid)
				MSDOS_I(old_inode)->i_attrs |= ATTR_HIDDEN;
			else
				MSDOS_I(old_inode)->i_attrs &= ~ATTR_HIDDEN;
			if (IS_DIRSYNC(old_dir)) {
				err = fat_sync_inode(old_inode);
				if (err) {
					MSDOS_I(old_inode)->i_attrs = old_attrs;
					goto out;
				}
			} else
				mark_inode_dirty(old_inode);

			old_dir->i_version++;
			old_dir->i_ctime = old_dir->i_mtime = CURRENT_TIME_SEC;
			if (IS_DIRSYNC(old_dir))
				(void)fat_sync_inode(old_dir);
			else
				mark_inode_dirty(old_dir);
			goto out;
		}
	}

	ts = CURRENT_TIME_SEC;
	if (new_inode) {
		if (err)
			goto out;
		if (is_dir) {
			err = fat_dir_empty(new_inode);
			if (err)
				goto out;
		}
		new_i_pos = MSDOS_I(new_inode)->i_pos;
		fat_detach(new_inode);
	} else {
		err = vxext_add_entry(new_dir, new_name, is_dir, is_hid, 0,
				      &ts, &sinfo);
		if (err)
			goto out;
		new_i_pos = sinfo.i_pos;
	}
	new_dir->i_version++;

	fat_detach(old_inode);
	fat_attach(old_inode, new_i_pos);
	if (is_hid)
		MSDOS_I(old_inode)->i_attrs |= ATTR_HIDDEN;
	else
		MSDOS_I(old_inode)->i_attrs &= ~ATTR_HIDDEN;
	if (IS_DIRSYNC(new_dir)) {
		err = fat_sync_inode(old_inode);
		if (err)
			goto error_inode;
	} else
		mark_inode_dirty(old_inode);

	if (update_dotdot) {
		int start = MSDOS_I(new_dir)->i_logstart;
		dotdot_de->start = cpu_to_le16(start);
      #ifndef VXEXT_FS
		dotdot_de->starthi = cpu_to_le16(start >> 16);
      #endif
		mark_buffer_dirty_inode(dotdot_bh, old_inode);
		if (IS_DIRSYNC(new_dir)) {
			err = sync_dirty_buffer(dotdot_bh);
			if (err)
				goto error_dotdot;
		}
		drop_nlink(old_dir);
		if (!new_inode)
			inc_nlink(new_dir);
	}

	err = fat_remove_entries(old_dir, &old_sinfo);	/* and releases bh */
	old_sinfo.bh = NULL;
	if (err)
		goto error_dotdot;
	old_dir->i_version++;
	old_dir->i_ctime = old_dir->i_mtime = ts;
	if (IS_DIRSYNC(old_dir))
		(void)fat_sync_inode(old_dir);
	else
		mark_inode_dirty(old_dir);

	if (new_inode) {
		drop_nlink(new_inode);
		if (is_dir)
			drop_nlink(new_inode);
		new_inode->i_ctime = ts;
	}
out:
	brelse(sinfo.bh);
	brelse(dotdot_bh);
	brelse(old_sinfo.bh);
	return err;

error_dotdot:
	/* data cluster is shared, serious corruption */
	corrupt = 1;

	if (update_dotdot) {
		int start = MSDOS_I(old_dir)->i_logstart;
		dotdot_de->start = cpu_to_le16(start);
      #ifndef VXEXT_FS
		dotdot_de->starthi = cpu_to_le16(start >> 16);
      #endif
		mark_buffer_dirty_inode(dotdot_bh, old_inode);
		corrupt |= sync_dirty_buffer(dotdot_bh);
	}
error_inode:
	fat_detach(old_inode);
	fat_attach(old_inode, old_sinfo.i_pos);
	MSDOS_I(old_inode)->i_attrs = old_attrs;
	if (new_inode) {
		fat_attach(new_inode, new_i_pos);
		if (corrupt)
			corrupt |= fat_sync_inode(new_inode);
	} else {
		/*
		 * If new entry was not sharing the data cluster, it
		 * shouldn't be serious corruption.
		 */
		int err2 = fat_remove_entries(new_dir, &sinfo);
		if (corrupt)
			corrupt |= err2;
		sinfo.bh = NULL;
	}
	if (corrupt < 0) {
		fat_fs_error(new_dir->i_sb,
			     "%s: Filesystem corrupted (i_pos %lld)",
			     __func__, sinfo.i_pos);
	}
	goto out;
}

/***** Rename, a wrapper for rename_same_dir & rename_diff_dir */
static int vxext_rename(struct inode *old_dir, struct dentry *old_dentry,
			struct inode *new_dir, struct dentry *new_dentry)
{
	struct super_block *sb = old_dir->i_sb;
	unsigned char old_msdos_name[MSDOS_NAME], new_msdos_name[MSDOS_NAME];
	int err, is_hid;

	lock_super(sb);

	err = vxext_format_name(old_dentry->d_name.name,
				old_dentry->d_name.len, old_msdos_name,
				&MSDOS_SB(old_dir->i_sb)->options);
	if (err)
		goto out;
	err = vxext_format_name(new_dentry->d_name.name,
				new_dentry->d_name.len, new_msdos_name,
				&MSDOS_SB(new_dir->i_sb)->options);
	if (err)
		goto out;

	is_hid =
	     (new_dentry->d_name.name[0] == '.') && (new_msdos_name[0] != '.');

	err = do_vxext_rename(old_dir, old_msdos_name, old_dentry,
			      new_dir, new_msdos_name, new_dentry, is_hid);
out:
	unlock_super(sb);
	if (!err)
		err = fat_flush_inodes(sb, old_dir, new_dir);
	return err;
}

static const struct inode_operations vxext_dir_inode_operations = {
	.create		= vxext_create,
	.lookup		= vxext_lookup,
	.unlink		= vxext_unlink,
	.mkdir		= vxext_mkdir,
	.rmdir		= vxext_rmdir,
	.rename		= vxext_rename,
	.setattr	= fat_setattr,
	.getattr	= fat_getattr,
};

static int vxext_fill_super(struct super_block *sb, void *data, int silent)
{
	int res;

	res = fat_fill_super(sb, data, silent, &vxext_dir_inode_operations, 0);
	if (res)
		return res;

	sb->s_flags |= MS_NOATIME;
	sb->s_root->d_op = &vxext_dentry_operations;
	return 0;
}

static int vxext_get_sb(struct file_system_type *fs_type,
			int flags, const char *dev_name,
			void *data, struct vfsmount *mnt)
{
	return get_sb_bdev(fs_type, flags, dev_name, data, vxext_fill_super,
			   mnt);
}

static struct file_system_type vxext_fs_type = {
	.owner		= THIS_MODULE,
	.name		= "vxext",
	.get_sb		= vxext_get_sb,
	.kill_sb	= kill_block_super,
	.fs_flags	= FS_REQUIRES_DEV,
};

static int __init init_vxext_fs(void)
{
   int err;

   err = fat_cache_init();
   if (err)
      return err;

   err = fat_init_inodecache();
   if (err)
      goto failed;

   return register_filesystem(&vxext_fs_type);

failed:
   fat_cache_destroy();
   return err;
}

static void __exit exit_vxext_fs(void)
{
   fat_cache_destroy();
   fat_destroy_inodecache();
   unregister_filesystem(&vxext_fs_type);
}

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Jens Langner <Jens.Langner@light-speed.de>");
MODULE_DESCRIPTION("VxWorks extended DOS filesystem support");
MODULE_VERSION("2.2");

module_init(init_vxext_fs)
module_exit(exit_vxext_fs)
