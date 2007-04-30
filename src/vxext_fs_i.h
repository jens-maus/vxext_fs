#ifndef _VXEXT_FS_I
#define _VXEXT_FS_I

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

#include <linux/fs.h>

/*
 * VxWorks extended DOS file system inode data in memory
 */

struct vxext_inode_info
{
	// cache of lastest accessed cluster
	int file_cluster;							// cluster number in the file.
	int disk_cluster;							// cluster number on disk.

	loff_t mmu_private;
	int i_start;									// first cluster or 0
	int i_logstart;								// logical first cluster
	int i_attrs;									// unused attribute bits
	loff_t i_pos;									// on-disk position of directory entry or 0
	struct list_head i_fat_hash;	// hash by i_location
	struct inode vfs_inode;
};

#endif
