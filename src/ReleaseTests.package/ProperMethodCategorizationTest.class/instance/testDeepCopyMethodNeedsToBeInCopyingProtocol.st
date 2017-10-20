tests - object
testDeepCopyMethodNeedsToBeInCopyingProtocol
	"The #deepCopy methods should be in method protocol 'copying'"
	
	self assureAll: #deepCopy areCategorizedIn: #copying  whenSubclassOf: Object
