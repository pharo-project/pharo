extended bytecode generation
genStoreRemoteInstanceVariable: instanceVariableIndex inObjectAt: tempIndex
	"*252	(3)	11111100 	kkkkkkkk	sjjjjjjj		Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access"

	(instanceVariableIndex < 0 or: [instanceVariableIndex > 65535]) ifTrue:
		[^self outOfRangeError: 'instanceVariableIndex' index: instanceVariableIndex range: 0 to: 65535].
	(tempIndex < 0 or: [tempIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempIndex range: 0 to: 127].
	instanceVariableIndex > 255 ifTrue:
		[self genUnsignedSingleExtendA: instanceVariableIndex // 256].
	stream
		nextPut: 252;
		nextPut: instanceVariableIndex \\ 256;
		nextPut: 1 << 7 + tempIndex