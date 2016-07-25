public-testing
isReadable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ (self permissions: aResolvable) isReadable