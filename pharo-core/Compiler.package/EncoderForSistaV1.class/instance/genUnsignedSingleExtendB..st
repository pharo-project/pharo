bytecode generation
genUnsignedSingleExtendB: extendedIndex
	(extendedIndex between: 0 and: 255) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: 0 to: 255].
	"225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)"
	stream
		nextPut: 225;
		nextPut: extendedIndex