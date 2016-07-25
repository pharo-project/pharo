rectangle functions
pointNearestTo: aPoint 
	"Return the point on my border closest to aPoint"
	| side |
	(self containsPoint: aPoint) 
		ifTrue: 
			[ side := self sideNearestTo: aPoint.
			side == #right ifTrue: [ ^ self right @ aPoint y ].
			side == #left ifTrue: [ ^ self left @ aPoint y ].
			side == #bottom ifTrue: [ ^ aPoint x @ self bottom ].
			side == #top ifTrue: [ ^ aPoint x @ self top ] ]
		ifFalse: [ ^ aPoint adhereTo: self ]