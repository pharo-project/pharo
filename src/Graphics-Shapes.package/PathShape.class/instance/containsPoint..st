testing
containsPoint: aPoint
	"Answer whether the receiver contains the given point."

	(self basicContainsPoint: aPoint) ifFalse: [^false].
	self segmentsDo: [:p1 :p2 |
		(aPoint onLineFrom: p1 to: p2 within: 0) ifTrue: [^true]].
	^false