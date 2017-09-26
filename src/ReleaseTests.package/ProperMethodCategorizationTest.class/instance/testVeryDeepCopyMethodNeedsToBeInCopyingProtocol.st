tests - object
testVeryDeepCopyMethodNeedsToBeInCopyingProtocol
	"The #veryDeepCopy methods should be in method protocol 'copying'"
	
	self assureAll: #veryDeepCopy areCategorizedIn: #copying  whenSubclassOf: Object
