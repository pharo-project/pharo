archive operations
extractAllTo: aDirectory
	"Extract all elements to the given directory"

	UIManager default
		informUserDuring: [ :bar | 
			bar max: self members size.
			self extractAllTo: aDirectory informing: bar ]