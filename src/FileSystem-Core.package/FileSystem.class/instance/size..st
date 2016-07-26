public
size: aResolvable
	"Resolve the argument and return the size for this file or directory "

	^ store size: (self resolve: aResolvable)