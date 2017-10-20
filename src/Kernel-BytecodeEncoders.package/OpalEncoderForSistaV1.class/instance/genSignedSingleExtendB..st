bytecode generation
genSignedSingleExtendB: extendedIndex
	(extendedIndex between: -128 and: 127) ifFalse:
		[^self outOfRangeError: 'index' index: extendedIndex range: -128 to: 127].
	"225		11100001	sbbbbbbb	Extend B (Ext B = Ext B prev * 256 + Ext B)"
	stream
		nextPut: 225;
		nextPut: (extendedIndex >= 0 ifTrue: [extendedIndex] ifFalse: [extendedIndex + 256]) 