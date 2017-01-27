public
modificationTimeOf: aResolvable
	"Returns the last date of modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)