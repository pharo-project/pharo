bytecode generation
genPushLiteralVar: literalIndex
	| extendedIndex |
	(literalIndex < 0 or: [literalIndex > 32768]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 32768].
	literalIndex < 16 ifTrue: 
		["16-31		0001 i i i i		Push Literal Variable #iiii"
		 stream nextPut: 16 + literalIndex.
		 ^self].
	"227		11100011	i i i i i i i i	Push Literal Variable #iiiiiiii (+ Extend A * 256)"
	(extendedIndex := literalIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 227;
		nextPut: extendedIndex