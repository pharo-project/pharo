primitives
primReadStart: fHandle fPosition: fPosition count: count
	"Start a read operation of count bytes starting at the given offset in the given file."

	<primitive: 'primitiveAsyncFileReadStart' module: 'AsynchFilePlugin'>
	self error: 'READ THE COMMENT FOR THIS METHOD.'

"NOTE: This method will fail if there is insufficient C heap to allocate an internal buffer of the required size (the value of count).  If you are trying to read a movie file, then the buffer size will be height*width*2 bytes.  Each Squeak image retains a value to be used for this allocation, and it it initially set to 0.  If you are wish to play a 640x480 movie, you need room for a buffer of 640*480*2 = 614400 bytes.  You should execute the following...

	Smalltalk extraVMMemory 2555000.

Then save-and-quit, restart, and try to open the movie file again.  If you are using Async files in another way, find out the value of count when this failure occurs (call it NNNN), and instead of the above, execute...

	Smalltalk extraVMMemory: Smalltalk extraVMMemory + NNNN

then save-and-quit, restart, and try again.
"

