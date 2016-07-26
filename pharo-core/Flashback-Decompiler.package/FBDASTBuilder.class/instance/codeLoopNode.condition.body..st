building
codeLoopNode: bool condition: seq1 body: seq2
	^ (self
		codeMessage: (#while , bool class name , ':') asSymbol
		receiver: (self codeBlock: seq1)
		arguments: {self codeBlock: seq2})
		propertyAt: #reconstructed put: true;
		yourself