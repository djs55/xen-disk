Attach virtual disks to Xen virtual machines
============================================

The xen-disk tool allows a virtual disk to be attached
dynamically to a running xen virtual machine. The tool
is intended for testing new block protocol extensions,
disk formats etc.

The virtual disk is represented by an OCaml module
implementing a simple signature containing 'read block',
'write block' operations. There are several existing modules
including

  1. DISCARD: this is used if no disk image file is
     given on the command-line. It acknowledges all requests
     but doesn't do anything. It is used for checking the
     performance of the disk protocol code.

  2. MMAP: this is used if a disk image file is given on
     the command-line with no format override. It calls mmap(2)
     on the disk image file and satisfies requests using
     memcpy()

  3. VHD: this is used if a disk image file is given on
     the command-line with the "vhd" format specified. It
     calls mmap(2) on the disk image file and assumes the file
     is in vhd format.

Example: testing the performance of the disk protocol code
----------------------------------------------------------

From domain 0 on a xen host running a VM named "debian", type:

    xen-disk connect debian

A new virtual device will be created inside the VM as /dev/xvdX.
Simple performance testing can be attempted via a command like:

    # dd if=/dev/xvdb of=/dev/null bs=1M iflag=direct count=100
    100+0 records in
    100+0 records out
    104857600 bytes (105 MB) copied, 0.125296 s, 837 MB/s

Example: attaching an existing file as a virtual disk
-----------------------------------------------------

In domain 0 create a disk file using a process similar to:

    dd if=/dev/zero of=disk.raw bs=1M count=1 seek=1K
    losetup /dev/loop0 disk.raw
    mkfs.ext3 /dev/loop0
    losetup -d /dev/loop0

Next attach it to a running VM using a command like:

    xen-disk connect debian --path disk.raw

A new virtual device will be created inside the VM as /dev/xvdX
containing an ext3 filesystem, which you can mount.

