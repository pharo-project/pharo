comparing
= aPragma
	self == aPragma
		ifTrue: [ ^ true ].
	self species == aPragma species
		ifFalse: [ ^ false ].
	self method = aPragma method
		ifFalse: [ ^ false ].
	self selector = aPragma selector
		ifFalse: [ ^ false ].
	self arguments = aPragma arguments
		ifFalse: [ ^ false ].
	^ true