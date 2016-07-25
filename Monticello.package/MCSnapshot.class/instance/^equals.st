comparing
= other
	self == other
		ifTrue: [ ^ true ].
	self species = other species
		ifFalse: [ ^ false ].
	^ definitions asArray = other definitions asArray