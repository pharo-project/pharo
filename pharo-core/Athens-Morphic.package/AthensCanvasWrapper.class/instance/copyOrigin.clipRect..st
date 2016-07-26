formcanvas copying
copyOrigin: aPoint clipRect: aRectangle
	^ self copy
		setOrigin: aPoint
		clipRect: (self clipRect intersect: aRectangle ifNone: [0@0 corner: 0@0])