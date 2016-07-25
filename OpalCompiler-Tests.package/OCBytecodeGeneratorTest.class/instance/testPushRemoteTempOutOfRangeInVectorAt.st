tests - errors
testPushRemoteTempOutOfRangeInVectorAt
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					pushRemoteTemp: 257 inVectorAt: 1 ] 
		raise: Error 