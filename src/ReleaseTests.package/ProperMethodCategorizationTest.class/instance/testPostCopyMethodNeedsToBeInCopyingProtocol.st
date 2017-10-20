tests - object
testPostCopyMethodNeedsToBeInCopyingProtocol
	"The #postCopy methods should be in method protocol 'copying'"
	
	self assureAll: #postCopy areCategorizedIn: #copying  whenSubclassOf: Object
