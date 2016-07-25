querying
hasDecoratorNamed: aKey
	^ self key = aKey or: [ next hasDecoratorNamed: aKey ]