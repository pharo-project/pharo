testing
containsPoint: aPoint
	(super containsPoint: aPoint)
		ifFalse: [ ^ false ].
	color isTransparent
		ifFalse: [ ^ (self filledForm pixelValueAt: aPoint - bounds topLeft + 1) > 0 ].
	self
		lineSegmentsDo: [ :p1 :p2 | 
			(aPoint onLineFrom: p1 to: p2 within: (3 max: (borderWidth + 1) // 2) asFloat)
				ifTrue: [ ^ true ] ].
	^ false 