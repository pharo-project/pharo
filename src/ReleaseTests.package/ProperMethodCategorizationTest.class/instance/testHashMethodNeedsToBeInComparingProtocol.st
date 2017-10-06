tests - object
testHashMethodNeedsToBeInComparingProtocol
	"The #hash method should be in method protocol 'comparing'"
	
	self assureAll: #hash areCategorizedIn: #comparing  whenSubclassOf: Object
