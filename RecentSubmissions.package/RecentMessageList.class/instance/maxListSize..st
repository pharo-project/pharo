accessing
maxListSize: aNumber

	maxListSize := aNumber negative ifTrue: [0] ifFalse:[aNumber].
	self checkSize
		ifTrue: [self updateView]