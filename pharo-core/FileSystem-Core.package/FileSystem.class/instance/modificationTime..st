public
modificationTime: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store modificationTime: (self resolve: aResolvable)