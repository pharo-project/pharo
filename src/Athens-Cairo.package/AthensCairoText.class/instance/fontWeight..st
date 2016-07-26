font description
fontWeight: anObject
	anObject isSymbol
		ifTrue:[ fontWeight := self weightFrom: anObject ]
		ifFalse:[ fontWeight :=  anObject ] 