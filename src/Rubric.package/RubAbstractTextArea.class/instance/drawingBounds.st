drawing
drawingBounds
	^ (self scrollPane isNil or: [ self wrapped ])
		ifTrue: [self innerBounds]
		ifFalse: [ self innerBounds topLeft extent: self class defaultMaxExtent @ self class defaultMaxExtent ]