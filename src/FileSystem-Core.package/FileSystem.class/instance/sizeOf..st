public
sizeOf: aResolvable
	"Resolve the argument and return the size for this file or directory "

	^ store sizeOf: (self resolve: aResolvable)