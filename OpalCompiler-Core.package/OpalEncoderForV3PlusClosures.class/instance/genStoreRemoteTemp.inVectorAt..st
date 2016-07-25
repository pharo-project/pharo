bytecode generation
genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"141 	10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 141;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]