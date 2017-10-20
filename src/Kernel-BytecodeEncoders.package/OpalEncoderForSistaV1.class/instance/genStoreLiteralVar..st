bytecode generation
genStoreLiteralVar: literalIndex
	"244		11110100	iiiiiiii		Store Literal Variable #iiiiiiii (+ Extend A * 256)"
	(literalIndex < 0 or: [literalIndex > 32768]) ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 32768].
	literalIndex > 255 ifTrue: 
		[self genUnsignedSingleExtendA: literalIndex // 256].
	stream
		nextPut: 244;
		nextPut: literalIndex \\ 256