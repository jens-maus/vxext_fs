/* vim:set ts=2 nowrap: ****************************************************

 VXEXT fs - VxWorks extended DOS filesystem support
 Copyright (c) 2004 by Jens Langner <Jens.Langner@light-speed.de>

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

#include <linux/slab.h>
#include <linux/time.h>
#include <linux/vxext_fs.h>
#include <linux/dirent.h>
#include <linux/smp_lock.h>
#include <linux/buffer_head.h>

#include <asm/uaccess.h>

struct file_operations vxext_dir_operations = 
{
	.read			= generic_read_dir,
	.readdir	= vxext_readdir,
	.fsync		= file_fsync,
};

int vxext_readdir(struct file *filp, void *dirent, filldir_t filldir)
{
	struct inode *inode = filp->f_dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	struct buffer_head *bh;
	struct vxext_dir_entry *de;	
	unsigned char bufname[40];
	unsigned long lpos, dummy, *furrfu = &lpos;
	unsigned long inum;
	int i;
	loff_t i_pos, cpos;
	int ret = 0;
	
	lock_kernel();

	cpos = filp->f_pos;

	// Fake . and .. for the root directory.
	if(inode->i_ino == VXEXT_ROOT_INO)
	{
		while(cpos < 2)
		{
			if(filldir(dirent, "..", cpos+1, cpos, VXEXT_ROOT_INO, DT_DIR) < 0)
				goto out;

			cpos++;
			filp->f_pos++;
		}

		if(cpos == 2)
		{
			dummy = 2;
			furrfu = &dummy;
			cpos = 0;
		}
	}

	if(cpos & (sizeof(struct vxext_dir_entry)-1))
	{
		ret = -ENOENT;
		goto out;
	}

 	bh = NULL;

GetNew:
	if(vxext_get_entry(inode,&cpos,&bh,&de,&i_pos) == -1)
		goto EODir;

	if((de->attr & ATTR_VOLUME) || IS_FREE(de->name))
		goto RecEnd;

	#ifdef DEBUG
	printk(KERN_INFO "DirEntry:\n");
	printk(KERN_INFO "Name.........: [%s]\n",de->name);
	printk(KERN_INFO "Attributes...: %x\n",  de->attr);
	printk(KERN_INFO "Time.........: %x\n",  CT_LE_W(de->time));
	printk(KERN_INFO "Date.........: %x\n",  CT_LE_W(de->date));
	printk(KERN_INFO "StartCluster.: %d\n",  CT_LE_W(de->start));
	printk(KERN_INFO "FileSize.....: %ld\n", CT_LE_L(de->size));
	#endif

	memcpy(bufname, de->name, sizeof(de->name));

	lpos = cpos - 1*sizeof(struct vxext_dir_entry);

	if(!memcmp(de->name, VXEXT_DOT, VXEXT_NAMELEN))
	{
		inum = inode->i_ino;
	}
	else if(!memcmp(de->name, VXEXT_DOTDOT, VXEXT_NAMELEN))
	{
		inum = parent_ino(filp->f_dentry);
	}
	else
	{
		struct inode *tmp = vxext_iget(sb, i_pos);

		if(tmp) 
		{
			inum = tmp->i_ino;
			iput(tmp);
		} 
		else
			inum = iunique(sb, VXEXT_ROOT_INO);
	}

	// find the end of the name
	for(i=0; bufname[i] && bufname[i] != ' ' ; i++);
	
	// make sure the end is properly terminated with a NUL
	bufname[i] = '\0';

	// now lets fill the directory tree
	if (filldir(dirent, bufname, i, *furrfu, inum,
	    (de->attr & ATTR_DIR) ? DT_DIR : DT_REG) < 0)
	{
		goto FillFailed;
	}

RecEnd:
	furrfu = &lpos;
	filp->f_pos = cpos;
	goto GetNew;
	
EODir:
	filp->f_pos = cpos;

FillFailed:
	if(bh)
		brelse(bh);

out:
	unlock_kernel();
	return ret;
}

/* This assumes that size of cluster is above the 32*slots */
int vxext_add_entries(struct inode *dir, int slots, struct buffer_head **bh,
											struct vxext_dir_entry **de, loff_t *i_pos)
{
	loff_t offset = 0;
	loff_t curr = 0;
	int row = 0;
	struct buffer_head *new_bh;

	*bh = NULL;

	while(vxext_get_entry(dir, &curr, bh, de, i_pos) > -1)
	{
		// check the maximum size of directory */
		if(curr >= FAT_MAX_DIR_SIZE)
		{
			brelse(*bh);
			return -ENOSPC;
		}

		if(IS_FREE((*de)->name))
		{
			if(++row == slots)
				return offset;
		}
		else
		{
			row = 0;
			offset = curr;
		}
	}

	if((dir->i_ino == VXEXT_ROOT_INO)) 
		return -ENOSPC;
		
	new_bh = vxext_extend_dir(dir);
	if (IS_ERR(new_bh))
		return PTR_ERR(new_bh);

	brelse(new_bh);

	do
	{
		vxext_get_entry(dir, &curr, bh, de, i_pos);
	}
	while(++row < slots);

	return offset;
}

int vxext_new_dir(struct inode *dir, struct inode *parent)
{
	struct buffer_head *bh;
	struct vxext_dir_entry *de;
	__u16 date, time;

	bh = vxext_extend_dir(dir);
	if (IS_ERR(bh))
		return PTR_ERR(bh);

	// zeroed out, so...
	vxext_date_unix2dos(dir->i_mtime.tv_sec,&time,&date);
	de = (struct vxext_dir_entry*)&bh->b_data[0];
  memcpy(de[0].name, VXEXT_DOT, VXEXT_NAMELEN);
  memcpy(de[1].name, VXEXT_DOTDOT, VXEXT_NAMELEN);
	de[0].attr = de[1].attr = ATTR_DIR;
	de[0].time = de[1].time = CT_LE_W(time);
	de[0].date = de[1].date = CT_LE_W(date);
	de[0].start = CT_LE_W(VXEXT_I(dir)->i_logstart);
	de[1].start = CT_LE_W(VXEXT_I(parent)->i_logstart);
	mark_buffer_dirty(bh);
	brelse(bh);
	dir->i_atime = dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	mark_inode_dirty(dir);

	return 0;
}

static int vxext_get_short_entry(struct inode *dir, loff_t *pos,
													       struct buffer_head **bh,
																 struct vxext_dir_entry **de,
																 loff_t *i_pos)
{
	while (vxext_get_entry(dir, pos, bh, de, i_pos) >= 0)
	{
		// free entry or long name entry or volume label
		if (!IS_FREE((*de)->name) && !((*de)->attr & ATTR_VOLUME))
			return 0;
	}
	
	return -ENOENT;
}

/* See if directory is empty */
int vxext_dir_empty(struct inode *dir)
{
	struct buffer_head *bh;
	struct vxext_dir_entry *de;
	loff_t cpos, i_pos;
	int result = 0;

	bh = NULL;
	cpos = 0;

	while(vxext_get_short_entry(dir, &cpos, &bh, &de, &i_pos) >= 0)
	{
	  if(strncmp(de->name, VXEXT_DOT, VXEXT_NAMELEN) &&
			 strncmp(de->name, VXEXT_DOTDOT, VXEXT_NAMELEN))
		{
			result = -ENOTEMPTY;
      break;
    }
	}

	brelse(bh);
	return result;
}

/*
 * vxext_subdirs counts the number of sub-directories of dir. It can be run
 * on directories being created.
 */
int vxext_subdirs(struct inode *dir)
{
	struct buffer_head *bh;
	struct vxext_dir_entry *de;
	loff_t cpos, i_pos;
	int count = 0;

	bh = NULL;
	cpos = 0;
	
	while(vxext_get_short_entry(dir, &cpos, &bh, &de, &i_pos) >= 0)
	{
		if(de->attr & ATTR_DIR)
			count++;
	}
	
	brelse(bh);
	return count;
}

/*
 * Scans a directory for a given file (name points to its formatted name).
 * Returns an error code or zero.
 */
int vxext_scan(struct inode *dir, const unsigned char *name,
							 struct buffer_head **bh, struct vxext_dir_entry **de,
							 loff_t *i_pos)
{
	loff_t cpos;

	*bh = NULL;
	cpos = 0;

	while(vxext_get_short_entry(dir, &cpos, bh, de, i_pos) >= 0)
	{
		if(!strncmp((*de)->name, name, VXEXT_NAMELEN))
			return 0;
	}
	
	return -ENOENT;
}
