accessing
= another

	self species = another species ifFalse: [ ^ false ].
	^ self label = another label and: [ self actionHolder = another actionHolder ]