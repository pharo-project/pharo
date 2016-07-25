events-processing
isInsideOfTriggerBrick: anEvent
	self triggerBrick 
		ifNil: [^ false]
		ifNotNil: [:aBrick | 
			anEvent withHandPositionDo: [ :position | ^ aBrick boundsInWorld containsPoint: position ] ]