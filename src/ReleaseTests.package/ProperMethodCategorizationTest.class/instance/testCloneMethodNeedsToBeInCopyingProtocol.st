tests - object
testCloneMethodNeedsToBeInCopyingProtocol
	"The #clone methods should be in method protocol 'copying'"
	
	self assureAll: #clone areCategorizedIn: #copying  whenSubclassOf: Object
