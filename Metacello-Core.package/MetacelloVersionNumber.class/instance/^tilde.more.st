comparing
~> aMetacelloVersionNumber

	aMetacelloVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aMetacelloVersionNumber and: [ self < aMetacelloVersionNumber approximateBase ]