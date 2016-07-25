tests - errors
testPushRemoteTempInVectorAtOutOfRange
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					pushRemoteTemp: 1 inVectorAt: 257 ] 
		raise: Error 
	