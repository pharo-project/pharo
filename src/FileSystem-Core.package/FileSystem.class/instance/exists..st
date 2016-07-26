public
exists: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory at that path, false if there is not."
	
	^ store exists: (self resolve: aResolvable)