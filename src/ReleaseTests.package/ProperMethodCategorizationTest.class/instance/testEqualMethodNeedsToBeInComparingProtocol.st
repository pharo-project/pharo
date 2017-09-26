tests - object
testEqualMethodNeedsToBeInComparingProtocol
	"The #= method should be in method protocol 'comparing'"
	
	self assureAll: #= areCategorizedIn: #comparing  whenSubclassOf: Object
