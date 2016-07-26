public
mimeTypesAt: aResolvable
	"Return the possible mime types for the given path."
	^ store mimeTypesAt: (self resolve: aResolvable)