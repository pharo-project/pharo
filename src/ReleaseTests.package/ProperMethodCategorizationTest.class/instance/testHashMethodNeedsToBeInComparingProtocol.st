tests - object
testHashMethodNeedsToBeInComparingProtocol
	"The #tearDown method in SUnit test classes should be in method protocol 'running'"
	
	self assureAll: #hash areCategorizedIn: #comparing  whenSubclassOf: Object
