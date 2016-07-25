USING FROM OSPROCESS
primCreatePipe
	"Create a pipe, and answer an array of two file handles (SQFile data structures in interp.c)
	for the pipe reader and writer."

	<primitive: 'primitiveCreatePipe' module: 'UnixOSProcessPlugin'>
	^ nil