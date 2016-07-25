bytecode generation
genUnsignedSingleExtendA: extendedIndex
	(extendedIndex between: 0 and: 255) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: 0 to: 255].
	"224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)"
	stream
		nextPut: 224;
		nextPut: extendedIndex