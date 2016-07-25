testing
includes: anObject

	anObject ifNil: [ ^includesNil ].
	^(array at: (anObject largeIdentityHash \\ 4096 + 1))
		ifNil: [ false ]
		ifNotNil: [ :list | list fuelPointsTo: anObject ]