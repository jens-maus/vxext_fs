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

#include <linux/fs.h>
#include <linux/vxext_fs.h>
#include <linux/buffer_head.h>

/*
 * vxext_fs_panic reports a severe file system problem and sets the file system
 * read-only. The file system can be made writable again by remounting it.
 */

static char panic_msg[512];

void vxext_fs_panic(struct super_block *s, const char *fmt, ...)
{
	int not_ro;
	va_list args;

	va_start (args, fmt);
	vsnprintf (panic_msg, sizeof(panic_msg), fmt, args);
	va_end (args);

	not_ro = !(s->s_flags & MS_RDONLY);
	if(not_ro)
		s->s_flags |= MS_RDONLY;

	printk(KERN_ERR "VXEXT: Filesystem panic (dev %s)\n"
	       "    %s\n", s->s_id, panic_msg);

	if(not_ro)
		printk(KERN_ERR "     File system has been set read-only\n");
}

void vxlock_fat(struct super_block *sb)
{
	down(&(VXEXT_SB(sb)->fat_lock));
}

void vxunlock_fat(struct super_block *sb)
{
	up(&(VXEXT_SB(sb)->fat_lock));
}

/*
 * vxext_add_cluster tries to allocate a new cluster and adds it to the
 * file represented by inode.
 */
int vxext_add_cluster(struct inode *inode)
{
	struct super_block *sb = inode->i_sb;
	int ret, count, limit, new_dclus, new_fclus, last;
	
	/* 
	 * We must locate the last cluster of the file to add this new
	 * one (new_dclus) to the end of the link list (the FAT).
	 *
	 * In order to confirm that the cluster chain is valid, we
	 * find out EOF first.
	 */
	last = new_fclus = 0;
	if (VXEXT_I(inode)->i_start)
	{
		int ret, fclus, dclus;

		ret = vxext_get_cluster(inode, FAT_ENT_EOF, &fclus, &dclus);
		if(ret < 0)
			return ret;

		new_fclus = fclus + 1;
		last = dclus;
	}

	/* find free FAT entry */
	vxlock_fat(sb);
	
	if(VXEXT_SB(sb)->free_clusters == 0)
	{
		vxunlock_fat(sb);
		return -ENOSPC;
	}

	limit = VXEXT_SB(sb)->clusters + 2;
	new_dclus = VXEXT_SB(sb)->prev_free + 1;
	for(count = 0; count < VXEXT_SB(sb)->clusters; count++, new_dclus++)
	{
		new_dclus = new_dclus % limit;
		if(new_dclus < 2)
			new_dclus = 2;

		ret = vxext_access(sb, new_dclus, -1);
		if(ret < 0)
		{
			vxunlock_fat(sb);
			return ret;
		}
		else if(ret == FAT_ENT_FREE)
			break;
	}

	if(count >= VXEXT_SB(sb)->clusters)
	{
		VXEXT_SB(sb)->free_clusters = 0;
		vxunlock_fat(sb);
		return -ENOSPC;
	}

	ret = vxext_access(sb, new_dclus, FAT_ENT_EOF);
	if(ret < 0)
	{
		vxunlock_fat(sb);
		return ret;
	}

	VXEXT_SB(sb)->prev_free = new_dclus;
	if(VXEXT_SB(sb)->free_clusters != -1)
		VXEXT_SB(sb)->free_clusters--;

	vxunlock_fat(sb);

	// add new one to the last of the cluster chain
	if(last)
	{
		ret = vxext_access(sb, last, new_dclus);
		if(ret < 0)
			return ret;

		vxext_cache_add(inode, new_fclus, new_dclus);
	}
	else
	{
		VXEXT_I(inode)->i_start = new_dclus;
		VXEXT_I(inode)->i_logstart = new_dclus;
		mark_inode_dirty(inode);
	}

	if(new_fclus != (inode->i_blocks / VXEXT_SB(sb)->sec_per_clus))
	{
		vxext_fs_panic(sb, "clusters badly computed (%d != %lu)",
			new_fclus, inode->i_blocks / VXEXT_SB(sb)->sec_per_clus);
		vxext_cache_inval_inode(inode);
	}

	inode->i_blocks += VXEXT_SB(sb)->cluster_size >> 9;

	return new_dclus;
}

struct buffer_head *vxext_extend_dir(struct inode *inode)
{
	struct super_block *sb = inode->i_sb;
	struct buffer_head *bh, *res = NULL;
	int nr, sec_per_clus = VXEXT_SB(sb)->sec_per_clus;
	sector_t sector, last_sector;

	if(inode->i_ino == VXEXT_ROOT_INO)
		return ERR_PTR(-ENOSPC);

	nr = vxext_add_cluster(inode);
	if(nr < 0)
		return ERR_PTR(nr);
	
	sector = ((sector_t)nr - 2) * sec_per_clus + VXEXT_SB(sb)->data_start;
	last_sector = sector + sec_per_clus;
	for( ; sector < last_sector; sector++)
	{
		if((bh = sb_getblk(sb, sector)))
		{
			memset(bh->b_data, 0, sb->s_blocksize);
			set_buffer_uptodate(bh);
			mark_buffer_dirty(bh);

			if (!res)
				res = bh;
			else
				brelse(bh);
		}
	}

	if(res == NULL)
		res = ERR_PTR(-EIO);
	
	if(inode->i_size & (sb->s_blocksize - 1))
	{
		vxext_fs_panic(sb, "Odd directory size");
		inode->i_size = (inode->i_size + sb->s_blocksize)
			& ~((loff_t)sb->s_blocksize - 1);
	}

	inode->i_size += VXEXT_SB(sb)->cluster_size;
	VXEXT_I(inode)->mmu_private += VXEXT_SB(sb)->cluster_size;

	return res;
}

// Linear day numbers of the respective 1sts in non-leap years.
static int day_n[] = { 0,31,59,90,120,151,181,212,243,273,304,334,0,0,0,0 };
// JanFebMarApr May Jun Jul Aug Sep Oct Nov Dec


extern struct timezone sys_tz;


// Convert a MS-DOS time/date pair to a UNIX date (seconds since 1 1 70).
int vxext_date_dos2unix(unsigned short time,unsigned short date)
{
	int month,year,secs;

	// first subtract and mask after that... Otherwise, if
	// date == 0, bad things happen
	month = ((date >> 5) - 1) & 15;
	year = date >> 9;
	secs = (time & 31)*2+60*((time >> 5) & 63)+(time >> 11)*3600+86400*
	    ((date & 31)-1+day_n[month]+(year/4)+year*365-((year & 3) == 0 &&
	    month < 2 ? 1 : 0)+3653);
			// days since 1.1.70 plus 80's leap day
	secs += sys_tz.tz_minuteswest*60;

	return secs;
}


// Convert linear UNIX date to a MS-DOS time/date pair.
void vxext_date_unix2dos(int unix_date,unsigned short *time,
												 unsigned short *date)
{
	int day,year,nl_day,month;

	unix_date -= sys_tz.tz_minuteswest*60;

	// Jan 1 GMT 00:00:00 1980. But what about another time zone?
	if(unix_date < 315532800)
		unix_date = 315532800;

	*time = (unix_date % 60)/2+(((unix_date/60) % 60) << 5)+
	    (((unix_date/3600) % 24) << 11);

	day = unix_date/86400-3652;
	year = day/365;
	if((year+3)/4+365*year > day)
		year--;

	day -= (year+3)/4+365*year;
	if(day == 59 && !(year & 3))
	{
		nl_day = day;
		month = 2;
	}
	else
	{
		nl_day = (year & 3) || day <= 59 ? day : day-1;
		for(month = 0; month < 12; month++)
		{
			if(day_n[month] > nl_day)
				break;
		}
	}

	*date = nl_day-day_n[month-1]+1+(month << 5)+(year << 9);
}


/* Returns the inode number of the directory entry at offset pos. If bh is
   non-NULL, it is brelse'd before. Pos is incremented. The buffer header is
   returned in bh.
   AV. Most often we do it item-by-item. Makes sense to optimize.
   AV. OK, there we go: if both bh and de are non-NULL we assume that we just
   AV. want the next entry (took one explicit de=NULL in vfat/namei.c).
   AV. It's done in vxext_get_entry() (inlined), here the slow case lives.
   AV. Additionally, when we return -1 (i.e. reached the end of directory)
   AV. we make bh NULL. 
 */

int vxext__get_entry(struct inode *dir, loff_t *pos,struct buffer_head **bh,
										 struct vxext_dir_entry **de, loff_t *i_pos)
{
	struct super_block *sb = dir->i_sb;
	struct vxext_sb_info *sbi = VXEXT_SB(sb);
	sector_t phys, iblock;
	loff_t offset;
	int err;

next:
	offset = *pos;
	if(*bh)
		brelse(*bh);

	*bh = NULL;
	iblock = *pos >> sb->s_blocksize_bits;
	err = vxext_bmap(dir, iblock, &phys);
	if(err || !phys)
		return -1;	// beyond EOF or error

	*bh = sb_bread(sb, phys);
	if(*bh == NULL) 
	{
		printk(KERN_ERR "VXEXT: Directory bread(block %llu) failed\n",
		       (unsigned long long)phys);

		// skip this block
		*pos = (iblock + 1) << sb->s_blocksize_bits;
		goto next;
	}

	offset &= sb->s_blocksize - 1;
	*pos += sizeof(struct vxext_dir_entry);
	*de = (struct vxext_dir_entry *)((*bh)->b_data + offset);
	*i_pos = ((loff_t)phys << sbi->dir_per_block_bits) + (offset >> VXEXT_DIR_BITS);

	return 0;
}
