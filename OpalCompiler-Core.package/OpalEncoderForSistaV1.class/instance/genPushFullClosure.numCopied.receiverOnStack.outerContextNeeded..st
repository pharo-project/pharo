extended bytecode generation
genPushFullClosure: compiledBlockLiteralIndex numCopied: numCopied receiverOnStack: receiverOnStack outerContextNeeded: outerContextNeeded
	"*	255		11111111	xxxxxxxx	siyyyyyy	push Closure Compiled block literal index xxxxxxxx (+ Extend A * 256) numCopied yyyyyy receiverOnStack: s = 1 ignoreOuterContext: i = 1"
	| extendedIndex |
	(numCopied < 1 or: [numCopied > 64]) ifTrue:
		[self outOfRangeError: 'num copied' index: numCopied range: 1 to: 64].
	(compiledBlockLiteralIndex < 0 or: [compiledBlockLiteralIndex > 32768]) ifTrue:
		[^self outOfRangeError: 'index' index: compiledBlockLiteralIndex range: 0 to: 32768].
	(extendedIndex := compiledBlockLiteralIndex) > 255 ifTrue:
		[self genUnsignedSingleExtendA: extendedIndex // 256.
		 extendedIndex := extendedIndex \\ 256].
	stream
		nextPut: 255;
		nextPut: extendedIndex;
		nextPut: receiverOnStack asBit << 7 + (outerContextNeeded not asBit << 6) + numCopied