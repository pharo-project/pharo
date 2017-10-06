tests - object
testFinalizeMethodNeedsToBeInComparingProtocol
	"The #finalize method should be in method protocol 'finalization'"
	
	self assureAll: #finalize areCategorizedIn: #finalization  whenSubclassOf: Object
