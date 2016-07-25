rounding
toggleCornerRounding
	self cornerStyle == #rounded
		ifTrue: [self cornerStyle: #square]
		ifFalse: [self cornerStyle: #rounded].
	self changed