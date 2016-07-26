accessing
next: count putAll: collection startingAt: offset
	"Write count characters from collection starting at offset."
	
	self encoder 
		next: count 
		putAll: collection 
		startingAt: offset 
		toStream: stream 