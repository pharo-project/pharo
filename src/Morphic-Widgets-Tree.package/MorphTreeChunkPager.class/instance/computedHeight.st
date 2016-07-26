accessing
computedHeight 
	^ self nodeList size > self lastIndex 
		ifTrue: [super computedHeight ]
		ifFalse:[0]