tests
testInitializeVariables
	self testingSharedPool 
		reset;
		extractAndStoreHeadersInformation.
	self assert: self testingSharedPool shouldInitializeVariables.
	self testingSharedPool initializeVariablesIfNeeded. 
	self assert: self testingSharedPool platformName equals: FFISharedPool currentPlatformName.
	self assert: self testingSharedPool wordSize equals: FFISharedPool currentWordSize.
	self assert: (self testingSharedPool classVarNamed: 'SIGKILL') equals: 9.
	self assert: (self testingSharedPool classVarNamed: 'EINVAL') equals: 22.
	self assert: (self testingSharedPool classVarNamed: 'WHATEVER') isNil.


	
	