tests
testResetAndTestingMethods
	self testingSharedPool classVarNamed: 'SIGKILL' put: 9.
	self testingSharedPool reset. 
	"Class variables values are not reset"
	self assert: (self testingSharedPool classVarNamed: 'SIGKILL') isNil not.
	self assert: self testingSharedPool platformName isNil.
	self assert: self testingSharedPool wordSize isNil.
	self deny: self testingSharedPool hasAlreadyBeenInitializedBefore.
	self assert: self testingSharedPool shouldInitializeVariables.