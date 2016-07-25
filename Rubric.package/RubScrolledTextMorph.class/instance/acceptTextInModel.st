model protocol
acceptTextInModel
	self model ifNil: [ ^ true ].
	 ^ self setTextSelector numArgs = 2
		ifTrue: [ self model perform: self setTextSelector with: self text with: self ]
		ifFalse: [ self model perform: self setTextSelector withEnoughArguments: (Array with: self text) ]