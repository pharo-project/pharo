stream out
next: count putAll: collection
	"Write count bytes from collection"
	
	self 
		next: count 
		putAll: collection 
		startingAt: 1