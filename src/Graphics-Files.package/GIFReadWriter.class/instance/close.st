stream access
close
	"Write terminator"
	self nextPut: Terminator.
	^super close