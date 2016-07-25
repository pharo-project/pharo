geometry updating
extentFromClientBottomRight: aPoint
	| w newExtent |
	self wrapped
		ifFalse: [ ^ self extent ].
	newExtent := aPoint.
	w := newExtent x max: self minimumExtent x.
	self container: (self container topLeft extent: (w @ newExtent y) truncated).
	self compose.
	^ newExtent x @ self extent y