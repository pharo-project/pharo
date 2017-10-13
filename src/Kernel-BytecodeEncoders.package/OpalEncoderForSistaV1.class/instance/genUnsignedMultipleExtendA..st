bytecode generation
genUnsignedMultipleExtendA: extendedIndex
	"224		11100000	aaaaaaaa	Extend A (Ext A = Ext A prev * 256 + Ext A)"
	extendedIndex > 255 ifTrue:
		[self genUnsignedMultipleExtendA: extendedIndex // 256].
	stream
		nextPut: 224;
		nextPut: extendedIndex \\ 256