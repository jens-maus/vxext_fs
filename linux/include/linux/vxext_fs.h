#ifndef _LINUX_VXEXT_FS_H
#define _LINUX_VXEXT_FS_H

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
 * The VxWorks extended DOS filesystem constants/structures
 */
#include <asm/byteorder.h>

#define SECTOR_SIZE	512							// sector size (bytes)
#define SECTOR_BITS	9								// log2(SECTOR_SIZE)
#define VXEXT_DPB	(VXEXT_DPS)				// dir entries per block
#define VXEXT_DPB_BITS	3						// log2(VXEXT_DPB)
#define VXEXT_DPS	(SECTOR_SIZE / sizeof(struct vxext_dir_entry))
#define VXEXT_DPS_BITS	3						// log2(VXEXT_DPS)

#define VXEXT_SUPER_MAGIC 0x5657		// VW 
#define VXEXT_SUPER_MAGIC_STRING "VXEXT1.0"

#define VXEXT_ROOT_INO	1	// == MINIX_ROOT_INO
#define VXEXT_DIR_BITS	6	// log2(sizeof(struct vxext_dir_entry))

// directory limit
#define FAT_MAX_DIR_ENTRIES	(65536)
#define FAT_MAX_DIR_SIZE	(FAT_MAX_DIR_ENTRIES << VXEXT_DIR_BITS)

#define ATTR_NONE    0	// no attribute bits
#define ATTR_RO      1  // read-only
#define ATTR_HIDDEN  2  // hidden
#define ATTR_SYS     4  // system
#define ATTR_VOLUME  8  // volume label
#define ATTR_DIR     16 // directory
#define ATTR_ARCH    32 // archived

// attribute bits that are copied "as is"
#define ATTR_UNUSED  (ATTR_VOLUME | ATTR_ARCH | ATTR_SYS | ATTR_HIDDEN)

// bits that are used by the Windows 95/Windows NT extended FAT
//#define ATTR_EXT     (ATTR_RO | ATTR_HIDDEN | ATTR_SYS | ATTR_VOLUME)

#define DELETED_FLAG 0xe5 // mark file as deleted when in name[0]
#define IS_FREE(n) (!*(n) || *(n) == DELETED_FLAG)

// valid file mode bits
#define VXEXT_VALID_MODE (S_IFREG | S_IFDIR | S_IRWXU | S_IRWXG | S_IRWXO)

// Convert attribute bits and a mask to the UNIX mode.
#define VXEXT_MKMODE(a, m) (m & (a & ATTR_RO ? S_IRUGO|S_IXUGO : S_IRWXUGO))

// Convert the UNIX mode to MS-DOS attribute bits.
#define VXEXT_MKATTR(m) ((m & S_IWUGO) ? ATTR_NONE : ATTR_RO)

#define VXEXT_NAMELEN 40	// maximum VXEXT1.0 name length
#define VXEXT_DOT    	".                                        " // "."
#define VXEXT_DOTDOT 	"..                                       " // ".."

// media of boot sector
#define VXEXT_VALID_MEDIA(x)	((0xF8 <= (x) && (x) <= 0xFF) || (x) == 0xF0)
#define VXEXT_FIRST_ENT(s, x)	(0xFF00 | (x))

// maximum number of clusters allowed on a VXEXT filesystem
#define MAX_FAT_VXEXT 0xFFFE

// bad cluster mark
#define BAD_FAT16 0xFFF7

// standard EOF marks
#define EOF_FAT16 0xFFFF

#define FAT_ENT_FREE    (0)
#define FAT_ENT_BAD     (BAD_FAT16)
#define FAT_ENT_EOF     (EOF_FAT16)

/*
 * Conversion from and to little-endian byte order. (no-op on i386/i486)
 *
 * Naming: Ca_b_c, where a: F = from, T = to, b: LE = little-endian,
 * BE = big-endian, c: W = word (16 bits), L = longword (32 bits)
 */

#define CF_LE_W(v) le16_to_cpu(v)
#define CF_LE_L(v) le32_to_cpu(v)
#define CT_LE_W(v) cpu_to_le16(v)
#define CT_LE_L(v) cpu_to_le32(v)

struct vxext_boot_sector 
{
	__u8	ignored[3];				// 0x00 - Boot strap short or near jump instruction
	__u8	system_id[8];			// 0x03 - SystemID string									(VXEXT1.0)
	__u8	sector_size[2];		// 0x0b - Bytes per Sector								(512)
	__u8	sec_per_clus;			// 0x0d - Sectors per Cluster							(0)
	__u16	reserved;					// 0x0e - Reserved Sectors								(1)
	__u8	fats;							// 0x10 - Number of FAT tables						(2)
	__u8	dir_entries[2];		// 0x11 - Max. Number of Rootdir entries	(1024)
	__u8	sectors[2];				// 0x13 - Number of Sectors - short       (==0)
	__u8	media;						// 0x15 - Media Format ID code						(0xF8)
	__u16	fat_length;				// 0x16 - Sectors per FAT									(256)
	__u16	secs_track;				// 0x18 - Sectors per Track								(171)
	__u16	heads;						// 0x1a - Number of Heads									(6)
	__u32	hidden;						// 0x1c - Number of hidden Sectors				(0)
	__u32	total_sect;				// 0x20 - Number of Sectors - long        (>0)

	__u8	physical_num;			// 0x24 - Physical driver number/ID
	__u8	boot_sig;					// 0x26 - Boot signature record
	__u32	vol_id;						// 0x27 - Binary volume ID number
	__u8	vol_label[11];		// 0x2b - Volume label string
};

struct vxext_dir_entry 
{
	__u8	name[VXEXT_NAMELEN];// 0x00 - Filename max. 40 chars
	__u8	reserved[13];				// 0x28 - Reserved
	__u8	attr;								// 0x35 - File attributes
	__u16	time;								// 0x36 - File creation time (ctime)
	__u16	date;								// 0x38 - File creation date (cdate)
	__u16	start;							// 0x40 - Starting cluster number
	__u32	size;								// 0x42 - File size (in bytes)
};

#ifdef __KERNEL__

#include <linux/buffer_head.h>
#include <linux/string.h>
#include <linux/nls.h>
#include <linux/vxext_fs_i.h>
#include <linux/vxext_fs_sb.h>

static inline struct vxext_sb_info *VXEXT_SB(struct super_block *sb)
{
	return sb->s_fs_info;
}

static inline struct vxext_inode_info *VXEXT_I(struct inode *inode)
{
	return container_of(inode, struct vxext_inode_info, vfs_inode);
}

// vxext/cache.c
extern int vxext_access(struct super_block *sb, int nr, int new_value);
extern int __vxext_access(struct super_block *sb, int nr, int new_value);
extern int vxext_bmap(struct inode *inode, sector_t sector, sector_t *phys);
extern void vxext_cache_init(struct super_block *sb);
extern void vxext_cache_lookup(struct inode *inode, int cluster, int *f_clu,
															 int *d_clu);
extern void vxext_cache_add(struct inode *inode, int f_clu, int d_clu);
extern void vxext_cache_inval_inode(struct inode *inode);
extern int vxext_get_cluster(struct inode *inode, int cluster,
														 int *fclus, int *dclus);
extern int vxext_free(struct inode *inode, int skip);

// vxext/dir.c
extern struct file_operations vxext_dir_operations;
extern int vxext_readdir(struct file *filp, void *dirent, filldir_t filldir);
extern int vxext_add_entries(struct inode *dir, int slots, struct buffer_head **bh,
														 struct vxext_dir_entry **de, loff_t *i_pos);
extern int vxext_new_dir(struct inode *dir, struct inode *parent);
extern int vxext_dir_empty(struct inode *dir);
extern int vxext_subdirs(struct inode *dir);
extern int vxext_scan(struct inode *dir, const unsigned char *name,
											struct buffer_head **res_bh,
											struct vxext_dir_entry **res_de, loff_t *i_pos);

// vxext/file.c
extern struct file_operations vxext_file_operations;
extern struct inode_operations vxext_file_inode_operations;
extern int vxext_get_block(struct inode *inode, sector_t iblock,
													 struct buffer_head *bh_result, int create);
extern void vxext_truncate(struct inode *inode);

// vxext/inode.c
extern void vxext_hash_init(void);
extern void vxext_attach(struct inode *inode, loff_t i_pos);
extern void vxext_detach(struct inode *inode);
extern struct inode *vxext_iget(struct super_block *sb, loff_t i_pos);
extern struct inode *vxext_build_inode(struct super_block *sb,
																			 struct vxext_dir_entry *de,
																			 loff_t i_pos, int *res);
extern void vxext_delete_inode(struct inode *inode);
extern void vxext_clear_inode(struct inode *inode);
extern void vxext_put_super(struct super_block *sb);
int vxext_fill_super(struct super_block *sb, void *data, int silent,
										struct inode_operations *fs_dir_inode_ops);
extern int vxext_statfs(struct super_block *sb, struct kstatfs *buf);
extern void vxext_write_inode(struct inode *inode, int wait);
extern int vxext_notify_change(struct dentry * dentry, struct iattr * attr);

// vxext/misc.c
extern void vxext_fs_panic(struct super_block *s, const char *fmt, ...);
extern void vxlock_fat(struct super_block *sb);
extern void vxunlock_fat(struct super_block *sb);
extern int vxext_add_cluster(struct inode *inode);
extern struct buffer_head *vxext_extend_dir(struct inode *inode);
extern int vxext_date_dos2unix(unsigned short time, unsigned short date);
extern void vxext_date_unix2dos(int unix_date, unsigned short *time,
																unsigned short *date);
extern int vxext__get_entry(struct inode *dir, loff_t *pos,
														struct buffer_head **bh,
														struct vxext_dir_entry **de, loff_t *i_pos);

static __inline__ int vxext_get_entry(struct inode *dir, loff_t *pos,
																			struct buffer_head **bh,
																			struct vxext_dir_entry **de,
																			loff_t *i_pos)
{
	// Fast stuff first
	if(*bh && *de &&
	   (*de - (struct vxext_dir_entry *)(*bh)->b_data) < VXEXT_SB(dir->i_sb)->dir_per_block - 1)
	{
		*pos += sizeof(struct vxext_dir_entry);
		(*de)++;
		(*i_pos)++;
		return 0;
	}

	return vxext__get_entry(dir, pos, bh, de, i_pos);
}

extern int __init vxext_init_inodecache(void);
extern void __exit vxext_destroy_inodecache(void);
extern void vxext_hash_init(void);

#endif /* __KERNEL__ */

#endif
