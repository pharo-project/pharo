"
This class represents a straight line segment between two points

Instance variables:
	start	<Point>	start point of the line
	end		<Point>	end point of the line

"
Class {
	#name : #LineSegment,
	#superclass : #Object,
	#instVars : [
		'start',
		'end'
	],
	#category : #'Balloon-Geometry'
}

{ #category : #'instance creation' }
LineSegment class >> controlPoints: anArray [
	"Create a new instance of the receiver from the given control points"
	anArray size = 2 ifTrue:[^LineSegment new initializeFrom: anArray].
	anArray size = 3 ifTrue:[^Bezier2Segment new initializeFrom: anArray].
	anArray size = 4 ifTrue:[^Bezier3Segment new initializeFrom: anArray].
	self error:'Unsupported'.
]

{ #category : #'instance creation' }
LineSegment class >> from: startPoint to: endPoint [
	^self new from: startPoint to: endPoint
]

{ #category : #geometry }
LineSegment class >> from: startPoint to: endPoint via: via [
	(startPoint = via or: [ endPoint = via ]) ifTrue: [ ^self new from: startPoint to: endPoint ].
	^Bezier2Segment from: startPoint to: endPoint via: via
]

{ #category : #geometry }
LineSegment class >> fromPoints: pts [
	^self from: pts first to: pts third via: pts second
]

{ #category : #utilities }
LineSegment class >> intersectFrom: startPt with: startDir to: endPt with: endDir [
	"Compute the intersection of two lines, e.g., compute alpha and beta for
		startPt + (alpha * startDir) = endPt + (beta * endDir).
	Reformulating this yields
		(alpha * startDir) - (beta * endDir) = endPt - startPt.
	or
		(alpha * startDir) + (-beta * endDir) = endPt - startPt.
	or
		(alpha * startDir x) + (-beta * endDir x) = endPt x - startPt x.
		(alpha * startDir y) + (-beta * endDir y) = endPt y - startPt y.
	which is trivial to solve using Cramer's rule. Note that since
	we're really only interested in the intersection point we need only
	one of alpha or beta since the resulting intersection point can be
	computed based on either one."
	| det deltaPt alpha |
	det := (startDir x * endDir y) - (startDir y * endDir x).
	det = 0.0 ifTrue:[^nil]. "There's no solution for it"
	deltaPt := endPt - startPt.
	alpha := (deltaPt x * endDir y) - (deltaPt y * endDir x).
	alpha := alpha / det.
	"And compute intersection"
	^startPt + (alpha * startDir)
]

{ #category : #'vector functions' }
LineSegment >> asBezier2Curves: err [
	^Array with: self
]

{ #category : #converting }
LineSegment >> asBezier2Points: error [
	^Array with: start with: start with: end
]

{ #category : #converting }
LineSegment >> asBezier2Segment [
	"Represent the receiver as quadratic bezier segment"
	^Bezier2Segment from: start to: end
]

{ #category : #converting }
LineSegment >> asBezier2Segments: error [
	"Demote a cubic bezier to a set of approximating quadratic beziers."
	| pts |
	pts := self asBezier2Points: error.
	^(1 to: pts size by: 3) collect:[:i| 
		Bezier2Segment from: (pts at: i) via: (pts at: i+1) to: (pts at: i+2)].

]

{ #category : #converting }
LineSegment >> asIntegerSegment [
	"Convert the receiver into integer representation"
	^self species from: start asIntegerPoint to: end asIntegerPoint
]

{ #category : #converting }
LineSegment >> asLineSegment [
	"Represent the receiver as a straight line segment"
	^self
]

{ #category : #converting }
LineSegment >> asTangentSegment [
	^LineSegment from: end-start to: end-start
]

{ #category : #'bezier clipping' }
LineSegment >> bezierClipCurve: aCurve [
	^self bezierClipCurve: aCurve epsilon: 1
]

{ #category : #'bezier clipping' }
LineSegment >> bezierClipCurve: aCurve epsilon: eps [
	"Compute the intersection of the receiver (a line) with the given curve using bezier clipping."
	| tMin tMax clip newCurve |
	clip := self bezierClipInterval: aCurve.
	clip ifNil:[^#()]. "no overlap"
	tMin := clip at: 1.
	tMax := clip at: 2.
	newCurve := aCurve curveFrom: tMin to: tMax.
	newCurve length < eps ifTrue:[^Array with: (aCurve valueAt: tMin + tMax * 0.5)].
	(tMin < 0.001 and:[tMax > 0.999]) ifTrue:[
		"Need to split aCurve before proceeding"
		| curve1 curve2 |
		curve1 := aCurve curveFrom: 0.0 to: 0.5.
		curve2 := aCurve curveFrom: 0.5 to: 1.0.
		^(curve1 bezierClipCurve: self epsilon: eps),
			(curve2 bezierClipCurve: self epsilon: eps).
	].
	^newCurve bezierClipCurve: self epsilon: eps.
]

{ #category : #'bezier clipping' }
LineSegment >> bezierClipInterval: aCurve [
	"Compute the new bezier clip interval for the argument,
	based on the fat line (the direction aligned bounding box) of the receiver.
	Note: This could be modified so that multiple clip intervals are returned.
	The idea is that for a distance curve like

			x		x
	tMax----	--\-----/---\-------
				x		x
	tMin-------------------------

	all the intersections intervals with tMin/tMax are reported, therefore
	minimizing the iteration count. As it is, the process will slowly iterate
	against tMax and then the curve will be split.
	"
	| nrm tStep pts eps inside tValue tMin tMax last lastV lastT lastInside next nextV nextT nextInside vMin vMax |
	eps := 0.00001.					"distance epsilon"
	nrm := (start y - end y) @ (end x - start x). "normal direction for (end-start)"

	"Map receiver's control point into fat line; compute vMin and vMax"
	vMin := vMax := nil.
	self controlPointsDo:[:pt| | vValue |
		vValue := (nrm x * pt x) + (nrm y * pt y). "nrm dotProduct: pt."
		vMin == nil	ifTrue:[	vMin := vMax := vValue]
					ifFalse:[vValue < vMin ifTrue:[vMin := vValue].
							vValue > vMax ifTrue:[vMax := vValue]]].
	"Map the argument into fat line; compute tMin, tMax for clip"
	tStep := 1.0 / aCurve degree.
	pts := aCurve controlPoints.
	last := pts at: pts size.
	lastV := (nrm x * last x) + (nrm y * last y). "nrm dotProduct: last."
	lastT := 1.0.
	lastInside := lastV+eps < vMin ifTrue:[-1] ifFalse:[lastV-eps > vMax ifTrue:[1] ifFalse:[0]].

	"Now compute new minimal and maximal clip boundaries"
	inside := false.	"assume we're completely outside"
	tMin := 2.0. tMax := -1.0. 	"clip interval"
	1 to: pts size do:[:i|
		next := pts at: i.
		nextV := (nrm x * next x) + (nrm y * next y). "nrm dotProduct: next."
		false ifTrue:[
			(nextV - vMin / (vMax - vMin)) printString displayAt: 0@ (i-1*20)].
		nextT := i-1 * tStep.
		nextInside := nextV+eps < vMin ifTrue:[-1] ifFalse:[nextV-eps > vMax ifTrue:[1] ifFalse:[0]].
		nextInside = 0 ifTrue:[
			inside := true.
			tValue := nextT.
			tValue < tMin ifTrue:[tMin := tValue].
			tValue > tMax ifTrue:[tMax := tValue].
		].
		lastInside = nextInside ifFalse:["At least one clip boundary"
			inside := true.
			"See if one is below vMin"
			(lastInside + nextInside <= 0) ifTrue:[
				tValue := lastT + ((nextT - lastT) * (vMin - lastV) / (nextV - lastV)).
				tValue < tMin ifTrue:[tMin := tValue].
				tValue > tMax ifTrue:[tMax := tValue].
			].
			"See if one is above vMax"
			(lastInside + nextInside >= 0) ifTrue:[
				tValue := lastT + ((nextT - lastT) * (vMax - lastV) / (nextV - lastV)).
				tValue < tMin ifTrue:[tMin := tValue].
				tValue > tMax ifTrue:[tMax := tValue].
			].
		].
		last := next.
		lastT := nextT.
		lastV := nextV.
		lastInside := nextInside.
	].
	inside
		ifTrue:[^Array with: tMin with: tMax]
		ifFalse:[^nil]
]

{ #category : #accessing }
LineSegment >> bounds [
	"Return the bounds containing the receiver"
	^(start min: end) corner: (start max: end)
]

{ #category : #'vector functions' }
LineSegment >> controlPoints [
	^{start. end}
]

{ #category : #'vector functions' }
LineSegment >> controlPointsDo: aBlock [
	aBlock value: start; value: end
]

{ #category : #'vector functions' }
LineSegment >> curveFrom: parameter1 to: parameter2 [
	"Create a new segment like the receiver but starting/ending at the given parametric values"
	| delta |
	delta := end - start.
	^self shallowCopy from: delta * parameter1 + start to: delta * parameter2 + start
]

{ #category : #private }
LineSegment >> debugDraw [
	^self debugDrawAt: 0@0.
]

{ #category : #private }
LineSegment >> debugDrawAt: offset [
	| canvas |
	canvas := Display getCanvas.
	canvas translateBy: offset during:[:aCanvas|
		self lineSegmentsDo:[:p1 :p2|
			aCanvas line: p1 rounded to: p2 rounded width: 1 color: Color black.
		].
	].
]

{ #category : #accessing }
LineSegment >> degree [
	^1
]

{ #category : #accessing }
LineSegment >> direction [
	^end - start
]

{ #category : #accessing }
LineSegment >> end [
	"Return the end point"
	^end
]

{ #category : #accessing }
LineSegment >> end: aPoint [
	end := aPoint
]

{ #category : #initialize }
LineSegment >> from: startPoint to: endPoint [
	"Initialize the receiver"
	start := startPoint.
	end := endPoint.
]

{ #category : #testing }
LineSegment >> hasZeroLength [
	"Return true if the receiver has zero length"
	^start = end
]

{ #category : #initialize }
LineSegment >> initializeFrom: controlPoints [
	controlPoints size = 2 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	end := controlPoints at: 2.
]

{ #category : #intersection }
LineSegment >> intersectionWith: anotherSegment [
	"Copied from LineIntersections>>intersectFrom:to:with:to:"
	| det deltaPt alpha beta pt1Dir pt2Dir |
	pt1Dir := end - start.
	pt2Dir := anotherSegment end - anotherSegment start.
	det := (pt1Dir x * pt2Dir y) - (pt1Dir y * pt2Dir x).
	deltaPt := anotherSegment start - start.
	alpha := (deltaPt x * pt2Dir y) - (deltaPt y * pt2Dir x).
	beta := (deltaPt x * pt1Dir y) - (deltaPt y * pt1Dir x).
	det = 0 ifTrue:[^nil]. "no intersection"
	alpha * det < 0 ifTrue:[^nil].
	beta * det < 0 ifTrue:[^nil].
	det > 0 
		ifTrue:[(alpha > det or:[beta > det]) ifTrue:[^nil]]
		ifFalse:[(alpha < det or:[beta < det]) ifTrue:[^nil]].
	"And compute intersection"
	^start + (alpha * pt1Dir / (det@det))
]

{ #category : #testing }
LineSegment >> isArcSegment [
	"Answer whether I approximate an arc segment reasonably well"
	| mid v1 v2 d1 d2 center |
	start = end ifTrue:[^false].
	mid := self valueAt: 0.5.
	v1 := (start + mid) * 0.5.
	v2 := (mid + end) * 0.5.
	d1 := mid - start. d1 := d1 y @ d1 x negated.
	d2 := end - mid.  d2 := d2 y @ d2 x negated.

	center := LineSegment
		intersectFrom: v1 with: d1 to: v2 with: d2.

	"Now see if the tangents are 'reasonably close' to the circle"
	d1 := (start - center) normalized dotProduct: self tangentAtStart normalized.
	d1 abs > 0.02 ifTrue:[^false].
	d1 := (end - center) normalized dotProduct: self tangentAtEnd normalized.
	d1 abs > 0.02 ifTrue:[^false].
	d1 := (mid - center) normalized dotProduct: self tangentAtMid normalized.
	d1 abs > 0.02 ifTrue:[^false].

	^true
]

{ #category : #testing }
LineSegment >> isBezier2Segment [
	"Return true if the receiver is a quadratic bezier segment"
	^false
]

{ #category : #testing }
LineSegment >> isLineSegment [
	"Return true if the receiver is a line segment"
	^true
]

{ #category : #testing }
LineSegment >> isStraight [
	"Return true if the receiver represents a straight line"
	^true
]

{ #category : #'vector functions' }
LineSegment >> length [
	"Return the length of the receiver"
	^start distanceTo: end
]

{ #category : #'vector functions' }
LineSegment >> lineSegments: steps do: aBlock [
	"Evaluate aBlock with the receiver's line segments"
	aBlock value: start value: end
]

{ #category : #'vector functions' }
LineSegment >> lineSegmentsDo: aBlock [
	"Evaluate aBlock with the receiver's line segments"
	aBlock value: start value: end
]

{ #category : #printing }
LineSegment >> printOn: aStream [
	"Print the receiver on aStream"
	aStream 
		nextPutAll: self class name;
		nextPutAll:' from: ';
		print: start;
		nextPutAll: ' to: ';
		print: end;
		space.
]

{ #category : #converting }
LineSegment >> reversed [
	^self class controlPoints: self controlPoints reversed
]

{ #category : #intersection }
LineSegment >> roundTo: quantum [
	start := start roundTo: quantum.
	end := end roundTo: quantum.
]

{ #category : #'vector functions' }
LineSegment >> sideOfPoint: aPoint [
	"Return the side of the receiver this point is on. The method returns
		-1: if aPoint is left
		 0: if aPoint is on
		+1: if a point is right
	of the receiver."
	| dx dy px py |
	dx := end x - start x.
	dy := end y - start y.
	px := aPoint x - start x.
	py := aPoint y - start y.
	^((dx * py) - (px * dy)) sign
"
	(LineSegment from: 0@0 to: 100@0) sideOfPoint: 50@-50.
	(LineSegment from: 0@0 to: 100@0) sideOfPoint: 50@50.
	(LineSegment from: 0@0 to: 100@0) sideOfPoint: 50@0.
"

]

{ #category : #accessing }
LineSegment >> start [
	"Return the start point"
	^start
]

{ #category : #accessing }
LineSegment >> start: aPoint [
	start := aPoint
]

{ #category : #'vector functions' }
LineSegment >> tangentAt: parameter [
	"Return the tangent at the given parametric value along the receiver"
	^end - start
]

{ #category : #'vector functions' }
LineSegment >> tangentAtEnd [
	"Return the tangent for the last point"
	^(end - start)
]

{ #category : #'vector functions' }
LineSegment >> tangentAtMid [
	"Return the tangent for the last point"
	^(end - start)
]

{ #category : #'vector functions' }
LineSegment >> tangentAtStart [
	"Return the tangent for the last point"
	^(end - start)
]

{ #category : #'vector functions' }
LineSegment >> valueAt: parameter [
	"Evaluate the receiver at the given parametric value"
	^start + (end - start * parameter)
]

{ #category : #'vector functions' }
LineSegment >> valueAtEnd [
	"Evaluate the receiver at it's end point (e.g., self valueAtEnd = (self valueAt: 1.0))"
	^end
]

{ #category : #'vector functions' }
LineSegment >> valueAtStart [
	"Evaluate the receiver at it's start point (e.g., self valueAtEnd = (self valueAt: 0.0))"
	^start
]
