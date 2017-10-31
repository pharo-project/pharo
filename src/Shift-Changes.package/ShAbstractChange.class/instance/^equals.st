comparing
= another
	self species = another species
		ifFalse: [ ^ false ].
	^ builder = another builder