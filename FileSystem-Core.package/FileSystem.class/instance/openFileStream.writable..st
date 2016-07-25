delegated
openFileStream: aResolvable writable: aBoolean
	^ store 
		openFileStream: (self resolve: aResolvable) 
		writable: aBoolean