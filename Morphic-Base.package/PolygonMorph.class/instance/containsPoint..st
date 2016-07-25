testing
containsPoint: aPoint
	(super containsPoint: aPoint) ifFalse: [^ false].

	(closed and: [color isTransparent not]) ifTrue:
		[ ^ (self filledForm pixelValueAt: aPoint - bounds topLeft + 1) > 0].

	self lineSegmentsDo:
		[ :p1 :p2 |
		(aPoint onLineFrom: p1 to: p2 within: (3 max: borderWidth+1//2) asFloat)
				ifTrue: [^ true]].

	self arrowForms do:
		[ :f | (f pixelValueAt: aPoint - f offset) > 0 ifTrue: [^ true]].

	^ false