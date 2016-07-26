brick-testing
isFitsInParent

	^ self owner
		"if our owner is a brick"
		ifNotNil: [ self owner globalBounds containsRect: self globalBounds ]
		"if our owner is a morph"
		ifNil: [ self owner ifNotNil: [ self owner bounds containsRect: self globalBounds ] ifNil: [ ^ true ] ]