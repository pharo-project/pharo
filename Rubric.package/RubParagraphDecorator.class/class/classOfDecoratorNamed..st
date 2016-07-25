querying
classOfDecoratorNamed: aKey 
	^ self allSubclasses detect: [ :cls | cls key = aKey ] ifNone: [  ]