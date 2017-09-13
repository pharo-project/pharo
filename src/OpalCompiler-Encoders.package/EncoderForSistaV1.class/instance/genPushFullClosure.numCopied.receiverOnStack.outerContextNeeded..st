extended bytecode generation
genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied receiverOnStack: receiverOnStack outerContextNeeded: outerContextNeeded
	"*	249		11111001 	xxxxxxxx	siyyyyyy	push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1"
	| extendedIndex |
	(numCopied < 0 or: [numCopied > 64]) ifTrue:
		[self outOfRangeError: 'num copied' index: numCopied range: 0 to: 64].
	(compiledBlockLiteralIndex < 0 or: [compiledBlockLiteralIndex > 32768]) ifTrue:
		[^self outOfRangeError: 'index' index: compiledBlockLiteralIndex range: 0 to: 32768].
	(extendedIndex := compiledBlockLiteralIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 249;
		nextPut: extendedIndex;
		nextPut: receiverOnStack asBit << 7 + (outerContextNeeded not asBit << 6) + numCopied