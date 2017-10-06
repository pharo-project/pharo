tests - object
testCopyMethodNeedsToBeInCopyingProtocol
	"The #copy methods should be in method protocol 'copying'"
	
	self assureAll: #copy areCategorizedIn: #copying  whenSubclassOf: Object
