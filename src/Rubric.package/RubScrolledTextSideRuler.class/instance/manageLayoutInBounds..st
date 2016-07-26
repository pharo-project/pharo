geometry
manageLayoutInBounds: aRectangle
	super manageLayoutInBounds: aRectangle.
	self extent: (self computedExtentFrom: aRectangle).
	self side = #left
		ifTrue: [ 
			self position: aRectangle topLeft.
			^ aRectangle topLeft + ((self computedWidthFrom: aRectangle) @ 0) corner: aRectangle corner ].
	self side = #right
		ifTrue: [ 
			self position: aRectangle topRight - ((self computedWidthFrom: aRectangle) @ 0).
			^ aRectangle topLeft corner: aRectangle corner - ((self computedWidthFrom: aRectangle) @ 0) ].
	self side = #bottom
		ifTrue: [ 
			self position: aRectangle bottomLeft - (0 @ (self computedHeightFrom: aRectangle)).
			^ aRectangle topLeft corner: aRectangle corner - (0 @ (self computedHeightFrom: aRectangle)) ].
	^ self error: 'side must be in #( #left #right #bottom #top #over )'