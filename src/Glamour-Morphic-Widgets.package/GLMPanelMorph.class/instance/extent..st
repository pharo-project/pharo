accessing
extent: aPoint
	| newExtent |	
	newExtent := aPoint rounded.
	(bounds extent closeTo: newExtent)
		ifTrue: [ ^ self ].

	self invalidRect: bounds.
	bounds := bounds topLeft extent: newExtent.
	self layoutChanged.
	self invalidRect: bounds