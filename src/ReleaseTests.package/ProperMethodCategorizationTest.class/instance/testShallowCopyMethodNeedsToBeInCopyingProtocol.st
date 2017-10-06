tests - object
testShallowCopyMethodNeedsToBeInCopyingProtocol
	"The #shallowCopy methods should be in method protocol 'copying'"
	
	self assureAll: #shallowCopy areCategorizedIn: #copying  whenSubclassOf: Object
