tests - errors
testStoreRemoteTempInVectorAtOutOfRange
	
	self should: [ self newBytecodeGen
					numArgs: 0;
					storeRemoteTemp: 1 inVectorAt: 257 ] 
		raise: Error 