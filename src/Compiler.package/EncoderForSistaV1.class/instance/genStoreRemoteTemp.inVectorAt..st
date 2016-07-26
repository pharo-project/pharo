bytecode generation
genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"252		11111100 	kkkkkkkk	jjjjjjjj		Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex < 0 or: [tempIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	(tempVectorIndex < 0 or: [tempVectorIndex >= 256]) ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255].
	stream
		nextPut: 252;
		nextPut: tempIndex;
		nextPut: tempVectorIndex