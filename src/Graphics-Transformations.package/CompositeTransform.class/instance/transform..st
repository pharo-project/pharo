transformations
transform: aPoint
	^ localTransform transform: (globalTransform transform: aPoint)