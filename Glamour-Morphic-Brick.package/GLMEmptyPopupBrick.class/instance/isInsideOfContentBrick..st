events-processing
isInsideOfContentBrick: anEvent
	self contentBrick 
		ifNil: [^ false]
		ifNotNil: [:aBrick | 
			anEvent withHandPositionDo: [ :position | ^ aBrick boundsInWorld containsPoint: position ]]