accessing
nextPutAll: collection
	"Write a collection"
	
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1