comparing
= definition
	self == definition
		ifTrue: [ ^ true ].
	self species = definition species
		ifFalse: [ ^ false ].
	^definition token = token