bytecode generation
genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"251		11111011 	kkkkkkkk	sjjjjjjj		Push Temp At kkkkkkkk In Temp Vector At: jjjjjjj, s = 1 implies remote inst var access instead of remote temp vector access"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 128]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 127].
	stream
		nextPut: 251;
		nextPut: tempIndex;
		nextPut: tempVectorIndex