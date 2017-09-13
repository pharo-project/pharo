bytecode generation
genStoreTemp: tempIndex
	"245		11110110	iiiiiiii		Store Temporary Variable #iiiiiiii"
	(tempIndex < 0 or: [tempIndex > 63]) ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	stream
		nextPut: 245;
		nextPut: tempIndex