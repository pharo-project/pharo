bytecode generation
genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"251		11111011 kkkkkkkk	jjjjjjjj		Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255].
	stream
		nextPut: 251;
		nextPut: tempIndex;
		nextPut: tempVectorIndex