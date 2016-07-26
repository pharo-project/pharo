path commands
reflectedCurveVia: cp2 to: aPoint
	"Reflected cubic bezier curve"
	
	| pt1 pt2 pt3 |

	pt2 := self toAbsolute: cp2.
	pt1 := lastSegment isCubic
		ifTrue: [ lastSegment via2reflected ]
		ifFalse: [ lastSegment endPoint ].
	pt3 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensCubicSegment new 
			from: lastSegment endPoint 
			via: pt1
			and: pt2
			to: pt3)

