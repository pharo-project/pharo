block encoding
sendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| result |
	result := 0.
	[literalStream atEnd] whileFalse:[
		result := result + (self privateSendBlock: literalStream
						with: distanceStream with: litTree with: distTree).
		self commit.
	].
	self nextBits: (litTree bitLengthAt: EndBlock) put: (litTree codeAt: EndBlock).
	^result