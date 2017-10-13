tests - object
testFinalizeValuesMethodNeedsToBeInComparingProtocol
	"The #finalizeValues methods should be in method protocol 'finalization'"
	
	self assureAll: #finalizeValues areCategorizedIn: #finalization  whenSubclassOf: Object
