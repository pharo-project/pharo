pipes
makeNonBlockingPipe
	"Creates a pipe with non blocking read"
	^ self makePipeWithReadBlocking: false