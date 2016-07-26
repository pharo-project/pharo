public
createDirectory: aResolvable
	"Resolve aResolvable into an absolute path, then as the store to create a directory there. 
	The store is expected to raise an exception if it cannot do so."

	^ store createDirectory: (self resolve: aResolvable)