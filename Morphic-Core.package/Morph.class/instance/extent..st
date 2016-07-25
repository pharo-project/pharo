geometry
extent: aPoint
	|newExtent|	
	newExtent := aPoint rounded.
	(bounds extent closeTo: newExtent) ifTrue: [^ self].
	self changed.
	bounds := (bounds topLeft extent: newExtent).
	self layoutChanged.
	self changed.
