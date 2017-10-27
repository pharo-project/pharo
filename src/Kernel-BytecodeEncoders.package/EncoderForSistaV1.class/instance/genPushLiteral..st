bytecode generation
genPushLiteral: literalIndex
	| extendedIndex |
	(literalIndex < 0 or: [literalIndex > 32768]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 32768].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	"228		11100100	i i i i i i i i	Push Literal #iiiiiiii (+ Extend A * 256)"
	(extendedIndex := literalIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 228;
		nextPut: extendedIndex