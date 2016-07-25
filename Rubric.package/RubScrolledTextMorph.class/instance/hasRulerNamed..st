rulers handling
hasRulerNamed: aKey
	^ self rulers anySatisfy: [ :r | r key = aKey ]
	