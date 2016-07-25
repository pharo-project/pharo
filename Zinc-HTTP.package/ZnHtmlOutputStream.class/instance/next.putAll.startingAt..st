streaming
next: count putAll: collection startingAt: offset
	"Write count characters from collection starting at offset."
	
	stream 
		next: count 
		putAll: collection 
		startingAt: offset