font description
fontSlant: anObject
	anObject isSymbol
		ifTrue:[ fontSlant := self slantFrom: anObject ]
		ifFalse:[ fontSlant := anObject ]