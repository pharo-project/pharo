tests
testChangePlatformAndTestingMethods
	self testingSharedPool reset; updatePlatformInfo.
	self deny: self testingSharedPool platformName isNil.
	self deny: self testingSharedPool wordSize isNil.
	self assert: self testingSharedPool hasAlreadyBeenInitializedBefore.
	self testingSharedPool platformName: 'whatever'. 
	self assert: self testingSharedPool shouldInitializeVariables.
	self assert: self testingSharedPool didPlatformChange.
	