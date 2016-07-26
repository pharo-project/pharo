comparing
= aKeymap
	self == aKeymap
		ifTrue: [ ^ true ].
	self species = aKeymap species
		ifFalse: [ ^ false ].
	^action = aKeymap action and: [ shortcut = aKeymap shortcut and: [ name = aKeymap name ] ]