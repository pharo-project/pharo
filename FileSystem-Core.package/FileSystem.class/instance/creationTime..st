public
creationTime: aResolvable
	"Answer the creation Time of the Resolvable"

	^ store creationTime: (self resolve: aResolvable)