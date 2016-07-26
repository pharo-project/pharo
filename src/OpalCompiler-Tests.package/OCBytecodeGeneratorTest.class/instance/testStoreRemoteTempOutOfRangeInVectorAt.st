tests - errors
testStoreRemoteTempOutOfRangeInVectorAt
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					storeRemoteTemp: 257 inVectorAt: 1 ] 
		raise: Error 