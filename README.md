File and vhd-backed block devices for Xen virtual machines
==========================================================

The vhddisk tool allows a block device to be attached
dynamically to a running virtual machine as follows:

vhddisk -file <filename> -type <file | vhd> -domain <domid | uuid | name>

