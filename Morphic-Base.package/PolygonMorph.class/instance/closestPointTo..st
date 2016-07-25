geometry
closestPointTo: aPoint 
	| closestPoint minDist |
	closestPoint := minDist := nil.
	self lineSegmentsDo: 
			[:p1 :p2 | | dist curvePoint | 
			curvePoint := aPoint nearestPointOnLineFrom: p1 to: p2.
			dist := curvePoint dist: aPoint.
			(closestPoint isNil or: [dist < minDist]) 
				ifTrue: 
					[closestPoint := curvePoint.
					minDist := dist]].
	^closestPoint