#ifndef _VXEXT_FS_SB
#define _VXEXT_FS_SB

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

/*
 * VxWorks extended DOS file system in-core superblock data
 */

struct vxext_mount_options 
{
	uid_t fs_uid;
	gid_t fs_gid;
	unsigned short fs_fmask;
	unsigned short fs_dmask;
	unsigned quiet:1;				 // set = fake successful chmods and chowns
};

#define FAT_CACHE_NR	8 // number of FAT cache

struct vxext_cache 
{
	int start_cluster;				// first cluster of the chain.
	int file_cluster;					// cluster number in the file.
	int disk_cluster;					// cluster number on disk.
	struct vxext_cache *next; // next cache entry
};

struct vxext_sb_info 
{
	unsigned short sec_per_clus; // sectors/cluster
	unsigned int cluster_size;   // cluster size
	unsigned char fats;					 // number of FATs
	unsigned short fat_start;
	unsigned long fat_length;    // FAT start & length (sec.)
	unsigned long dir_start;
	unsigned short dir_entries;  // root dir start & entries
	unsigned long data_start;    // first data sector
	unsigned long clusters;      // number of clusters
	unsigned long root_cluster;  // first cluster of the root directory
	struct semaphore fat_lock;
	int prev_free;               // previously allocated cluster number
	int free_clusters;           // -1 if undefined
	struct vxext_mount_options options;
	void *dir_ops;							 // Opaque; default directory operations
	int dir_per_block;					 // dir entries per block
	int dir_per_block_bits;	     // log2(dir_per_block)

	spinlock_t cache_lock;
	struct vxext_cache cache_array[FAT_CACHE_NR], *cache;
};

#endif
