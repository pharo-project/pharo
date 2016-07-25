I am a facade class to access standard output streams:

- standard input
- standard output
- standard error

I initialize standard streams in a lazy fashion, asking to the VM for the #stdioHandles. I cache those standard streams and release them on shutdown.

Known Bugs / subtleties
=======================

Windows VMs do now support the stdioHandles primitive if they are not compiled specially as command line binaries. To overcome that, on windows this class will create a normal file for std output / input / error.