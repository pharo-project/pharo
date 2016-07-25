visiting
visitSequence: instructionSequence
	self currentScope isPushClosureCopy ifTrue: [ 
		self currentScope destination = instructionSequence ifTrue: [ self popScope ] ].
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence
