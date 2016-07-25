composing
compositionRectangle
	^ scrollPane ifNil: [ self innerBounds ] ifNotNil: [ self innerBounds topLeft extent: scrollPane scrollBounds extent ]