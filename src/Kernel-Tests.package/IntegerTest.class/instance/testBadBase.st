tests - printing
testBadBase
	"This used to get into an endless loop.
	See Pharo #114"
	
	self should: [2 printStringBase: 1] raise: Error.