VXEXT FileSystem
================
The files you find in this directory are part of a reverse engineered 
implementation of the so called VXEXT1.0 extended DOS filesystem shipped and
used in the VxWorks Real-Time Operating System developed by Wind River, Inc.

Sources and development of this filesystem is largly based on the standard
FAT and MSDOS filesystem routines found as part of the main linux kernel
sources which are copyright by their respecitive authors mentioned there.
However, minor cosmetic changes have been made to the sources and non required
parts were removed wherever possible, making the sources easier to maintain.

AUTHOR
------
Development started in July 2004 and a first 1.0 version was published in
early August 2004. The current author is:

  Jens Maus – mail@jens-maus.de, http://jens-maus.de/

INSTALLATION
------------
Installation of this filesystem is mainly based on the way you want to
integrate it into the linux kernel:

(1) Compilation/Installation outside the kernel tree as a module (*.ko):
  1. Make sure you have installed the correct kernel sources for your currently running linux system.
  2. Unarchive the vxext_fs-x.x.tar.bz2 and cd into its subdirectory
  3. run `make` and cross your fingers!
  4. copy the final vxext.ko kernel module into the modules directory: `cp vxext.ko /lib/modules/$(uname -r)/misc`
  5. run `depmod -a`
  6. run `modprobe vxext` to load the kernel module
  7. mount your disk with `mount -t vxext /dev/sda`

(2) Direct integration into the linux kernel tree and compilation as either a module or compiled in filesystem:
  1. Install the proper kernel source tree in /usr/src/linux
  2. copy the vexet_fs_1_0-linux-X.X.X.patch patchfile to /usr/src/linux
  3. change to the /usr/src/linux directory
  4. run `patch -p1 <vxext_fs_1_0-linux.X.X.X.patch` to patch your kernel tree
  5. run `make menuconfig` and configure the VXEXT filesystem either as a module or directly compiled into the kernel.
  6. run `make ; make modules_install` in /usr/src/linux
  7. reboot and have phun.
  8. mount your disk with `mount -t vxext /dev/sda`

COPYRIGHT/LICENSE/WARRANTIES
----------------------------
The sources and all other documents found in this archive are free software;
you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more details.

VxWorks is a registered trademark of Wind River, Inc.

COMMENTS
--------
Please note that as this is a completley reverse engineered filesystem driver based
on the results of a VxWorks 5.2 system no warranty can be given that it might
also work with later versions of VxWorks or any other brand of the VxWorks RTOS.
Absolutly no documentation or official help from Wind River Inc was received 
during the development of this implementation. So the implementation is only
based on the research done during development but using disk analyzing tools and
such. So if you have any detailed official information about the exact implementation
of this filesystem you can share, please feel free to contact me.

MORE INFORMATION
----------------
Please consult the linux based vxext.txt filesystem documentation included in the
driver for a more detailed information on the particular implemenation.
