public
modificationTime: aResolvable
	"Returns the date of the last modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)