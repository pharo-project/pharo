printing
indicator
	"Returns a string indicating the type of reference:
	- '?' for a non existing reference',
	- '/' for a directory,
	- the empty string for a file."
	"When this framework gets more complete, it is possible to extend this behavior with the following indicators (taken from unix ls utility):
	- '*' for a regular file that is executable
	- '@' for a symbolic link
	- '|' for FIFOs
	- '=' for sockets
	- '>' for doors"
	^ self exists
		ifTrue: [self isDirectory ifTrue: ['/'] ifFalse: ['']  ]
		ifFalse: ['?']