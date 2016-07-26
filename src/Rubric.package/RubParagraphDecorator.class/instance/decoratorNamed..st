querying
decoratorNamed: aKey
	^ self key = aKey
		ifTrue: [ self ]
		ifFalse: [ next decoratorNamed: aKey ]