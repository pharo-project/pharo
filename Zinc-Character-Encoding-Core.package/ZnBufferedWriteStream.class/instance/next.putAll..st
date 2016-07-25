accessing
next: count putAll: collection
	"Write count elements from collection"
	
	self 
		next: count 
		putAll: collection 
		startingAt: 1