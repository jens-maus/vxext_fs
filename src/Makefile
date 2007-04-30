#/* vim:set ts=2 nowrap: ****************************************************
#
# VXEXT fs - VxWorks extended DOS filesystem support
# Copyright (c) 2004-2007 by Jens Langner <Jens.Langner@light-speed.de>
#
# This filesystem module is a reverse engineered implementation of the so
# called VXEXT1.0 extended DOS filesystem shipped with the VxWorks 5.2+
# RTOS operating system. The sources are largly based on the FAT and MSDOS
# filesystem routines found in the main Linux kernel sources which are
# copyright by their respecitive authors. However, minor cosmetic changes
# have been made and non-required parts were removed wherever possible.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# $Id$
#
#***************************************************************************/

ifneq ($(KERNELRELEASE),)

  obj-m := vxext.o

	vxext-y := cache.o \
	           dir.o   \
					   file.o  \
					   inode.o \
					   misc.o  \
					   namei.o

else

	KDIR := /lib/modules/$(shell uname -r)/build
	PWD  := $(shell pwd)

default:
	$(MAKE) -C $(KDIR) SUBDIRS=$(PWD) modules

endif

clean:
	rm -f *.o
	rm -f *.ko
	rm -f *.mod.c
	rm -f .*.cmd
	rm -f Module.symvers
	rm -rf .tmp_versions
