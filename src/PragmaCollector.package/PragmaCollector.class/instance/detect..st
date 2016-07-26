enumerating
detect: aBlock
	^ self collected detect: aBlock ifNone:[]
	