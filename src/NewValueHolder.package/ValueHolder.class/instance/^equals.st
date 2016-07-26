accessing
= another

	self species = another species ifFalse: [ ^ false ].
	^ self contents = another contents