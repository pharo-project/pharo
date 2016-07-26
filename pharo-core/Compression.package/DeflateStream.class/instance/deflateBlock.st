deflating
deflateBlock
	"Deflate the current contents of the stream"
	| flushNeeded lastIndex |
	(blockStart == nil) ifTrue:[
		"One time initialization for the first block"
		1 to: MinMatch-1 do:[:i| self updateHashAt: i].
		blockStart := 0].

	[blockPosition < position] whileTrue:[
		(position + MaxMatch > writeLimit)
			ifTrue:[lastIndex := writeLimit - MaxMatch]
			ifFalse:[lastIndex := position].
		flushNeeded := self deflateBlock: lastIndex-1
							chainLength: self hashChainLength
							goodMatch: self goodMatchLength.
		flushNeeded ifTrue:[
			self flushBlock.
			blockStart := blockPosition].
		"Make room for more data"
		self moveContentsToFront].
