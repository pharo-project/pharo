public
permissions: aResolvable
	"Resolve the argument and return the Permissions for this file or directory "

	^ store permissions: (self resolve: aResolvable)