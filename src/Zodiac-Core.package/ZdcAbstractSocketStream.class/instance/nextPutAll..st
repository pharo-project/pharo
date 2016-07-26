stream out
nextPutAll: collection
	"Write a collection of bytes"
	
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1