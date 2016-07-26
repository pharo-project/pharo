drawing
drawingBounds
	^ (self scrollPane isNil or: [ self wrapped ])
		ifTrue: [self innerBounds]
		ifFalse: [ self innerBounds topLeft extent: SmallInteger maxVal @ SmallInteger maxVal ]