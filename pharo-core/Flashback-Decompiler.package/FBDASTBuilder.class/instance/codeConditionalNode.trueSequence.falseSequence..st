building
codeConditionalNode: expr trueSequence: seq1 falseSequence: seq2
	^ (self
		codeMessage: #ifTrue:ifFalse:
		receiver: expr
		arguments:
			{self codeBlock: seq1.
			self codeBlock: seq2})
		propertyAt: #reconstructed put: true;
		yourself