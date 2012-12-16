Xen memory sharing and event notifications
==========================================

Xen virtual devices use "grant tables" to explicitly share
memory between domains and "event channels" to signal changes
to datastructures in shared memory.

This repository contains Linux userspace "gntdev" bindings
to manipulate grant tables and "evtchn" bindings to signal
other domains (VMs).

This code can be used (for example) to implement a disk block
server (a "block backend" or "blkback") capable of providing
a VM with a virtual disk.

The future
----------

Possibly the [openmirage](http://www.openmirage.org/) xen
kernel-space grant table and event channel support should
be merged in here, and this made a dependency of the
mirage-platform kernel. At the very least the interface
needs to be kept the same so we can share the same backend
server code between both user and kernel spaces.
