converting
asUTC

	^ offset isZero
		ifTrue: [ self ]
		ifFalse: [ self offset: 0 ]
