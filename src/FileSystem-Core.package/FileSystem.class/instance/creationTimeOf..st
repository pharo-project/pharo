public
creationTimeOf: aResolvable
	"Returns the creation date of aResolvable"

	^ store creationTimeOf: (self resolve: aResolvable)