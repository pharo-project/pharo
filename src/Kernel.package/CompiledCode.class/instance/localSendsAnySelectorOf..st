testing
localSendsAnySelectorOf: aCollection
	
	self literalsDo: [ :lit |
		(lit isSymbol and: [ aCollection includes: lit ]) 
			ifTrue: [ ^ self messages includesAny: aCollection ]
	].

	^ false 
