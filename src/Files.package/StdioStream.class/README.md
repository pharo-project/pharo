I am a concrete subclass of AbstractBinaryFileStream for stdio streams. I cannot modify the position of the underlying file.

Warning: Do not use me! You can access stdio streams through Stdio interface:

Stdio stdin.
Stdio stdout.
Stdio stderr.