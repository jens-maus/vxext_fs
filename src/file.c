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

#include <linux/time.h>
#include <linux/smp_lock.h>
#include <linux/buffer_head.h>

#include "vxext_fs.h"

struct file_operations vxext_file_operations =
{
	.llseek		 = generic_file_llseek,
	.read			 = do_sync_read,
	.write		 = do_sync_write,
  .aio_read  = generic_file_aio_read,
  .aio_write = generic_file_aio_write,
	.mmap			 = generic_file_mmap,
	.fsync		 = file_fsync,
	.sendfile	 = generic_file_sendfile,
};

struct inode_operations vxext_file_inode_operations =
{
	.truncate	= vxext_truncate,
	.setattr	= vxext_notify_change,
};

int vxext_get_block(struct inode *inode, sector_t iblock,
										struct buffer_head *bh_result, int create)
{
	struct super_block *sb = inode->i_sb;
	sector_t phys;
	int err;

	err = vxext_bmap(inode, iblock, &phys);
	if(err)
		return err;

	if(phys)
	{
		map_bh(bh_result, sb, phys);
		return 0;
	}

	if(!create)
		return 0;

	if(iblock != VXEXT_I(inode)->mmu_private >> sb->s_blocksize_bits)
	{
		vxext_fs_panic(sb, "corrupted file size (i_pos %lld, %lld)",
			     VXEXT_I(inode)->i_pos, VXEXT_I(inode)->mmu_private);

		return -EIO;
	}

	if(!((unsigned long)iblock & (VXEXT_SB(sb)->sec_per_clus - 1)))
	{
		int error;

		error = vxext_add_cluster(inode);
		if (error < 0)
			return error;
	}

	VXEXT_I(inode)->mmu_private += sb->s_blocksize;
	err = vxext_bmap(inode, iblock, &phys);
	if (err)
		return err;
		
	if(!phys)
		BUG();

	set_buffer_new(bh_result);
	map_bh(bh_result, sb, phys);
	return 0;
}

void vxext_truncate(struct inode *inode)
{
	struct vxext_sb_info *sbi = VXEXT_SB(inode->i_sb);
	const unsigned int cluster_size = sbi->cluster_size;
	int nr_clusters;

	// Why no return value?  Surely the disk could fail...
	if(IS_RDONLY (inode))
		return; // -EPERM

	if(IS_IMMUTABLE(inode))
		return; // -EPERM

	// This protects against truncating a file bigger than it was then
	// trying to write into the hole.
	if(VXEXT_I(inode)->mmu_private > inode->i_size)
		VXEXT_I(inode)->mmu_private = inode->i_size;

	nr_clusters = ((unsigned long)(inode->i_size + (cluster_size - 1))) / sbi->cluster_size;

	// again we have to round up the number of clusters because VXEXT isn't bound
	// to cluster sizes based on power 2.
	if(((unsigned long)(inode->i_size + (cluster_size - 1))) % sbi->cluster_size)
		nr_clusters++;
	
	lock_kernel();
	vxext_free(inode, nr_clusters);
	VXEXT_I(inode)->i_attrs |= ATTR_ARCH;
	unlock_kernel();
	inode->i_ctime = inode->i_mtime = CURRENT_TIME;
	mark_inode_dirty(inode);
}
