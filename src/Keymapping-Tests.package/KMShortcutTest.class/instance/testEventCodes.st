tests
testEventCodes
	self assert: $s meta modifier eventCode 
		 equals: OSPlatform current defaultModifier eventCode 