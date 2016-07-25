querying
classOfRulerNamed: aKey 
	^ self allSubclasses detect: [ :cls | cls key = aKey ] ifNone: [  ]