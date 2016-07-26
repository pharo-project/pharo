private
openStreamDescription: aResolvable writable: aBoolean
	"I am  a helper method to delegate basicOpen:writable: to the store.
	 I am called from FileSystemHandle implementations."
	
	| path |
	
	path := self resolve: aResolvable.
	^ store basicOpen: path writable: aBoolean