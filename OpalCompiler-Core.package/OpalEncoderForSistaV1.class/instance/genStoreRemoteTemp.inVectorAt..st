bytecode generation
genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"*252	(3)	11111100 	kkkkkkkk	sjjjjjjj		Store Temp At kkkkkkkk In Temp Vector At: jjjjjjj s = 1 implies remote inst var access instead of remote temp vector access"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 127].
	stream
		nextPut: 252;
		nextPut: tempIndex;
		nextPut: tempVectorIndex