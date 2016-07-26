instance creation
newForCapacity: capacity
	"Create an instance with the given capacity which must be a power of two."
	
	^(self basicNew: capacity) initialize: capacity
