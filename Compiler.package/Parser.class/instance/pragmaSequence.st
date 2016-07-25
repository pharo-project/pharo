pragmas
pragmaSequence
	"Parse a sequence of method pragmas."
	
	[ true ] whileTrue: [
		(self matchToken: #<)
			ifFalse: [ ^ self ].
		self pragmaStatement.
		(self matchToken: #>)
			ifFalse: [ ^ self expected: '>' ] ]