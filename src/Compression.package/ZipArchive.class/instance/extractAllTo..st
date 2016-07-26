archive operations
extractAllTo: aDirectory 
	"Extract all elements to the given directory"
	UIManager default informUserDuring: 
		[ :bar | 
		self 
			extractAllTo: aDirectory
			informing: bar ]