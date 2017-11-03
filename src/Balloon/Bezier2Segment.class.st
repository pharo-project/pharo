"
This class represents a quadratic bezier segment between two points

Instance variables:
	via		<Point>	The additional control point (OFF the curve)
"
Class {
	#name : #Bezier2Segment,
	#superclass : #LineSegment,
	#instVars : [
		'via'
	],
	#category : #Balloon-Geometry
}

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint to: endPoint via: viaPoint [
	^self new from: startPoint to: endPoint via: viaPoint
]

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint to: endPoint withMidPoint: pointOnCurve [
	^self new from: startPoint to: endPoint withMidPoint: pointOnCurve
]

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint to: endPoint withMidPoint: pointOnCurve at: parameter [
	^self new from: startPoint to: endPoint withMidPoint: pointOnCurve at: parameter
]

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint via: viaPoint to: endPoint [ 
	^self new from: startPoint to: endPoint via: viaPoint
]

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint withMidPoint: pointOnCurve at: parameter to: endPoint [ 
	^self new from: startPoint to: endPoint withMidPoint: pointOnCurve at: parameter
]

{ #category : #'instance creation' }
Bezier2Segment class >> from: startPoint withMidPoint: pointOnCurve to: endPoint [ 
	^self new from: startPoint to: endPoint withMidPoint: pointOnCurve
]

{ #category : #utilities }
Bezier2Segment class >> makeEllipseSegments: aRectangle [
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates eight bezier segments (two for each quadrant) approximating the oval."
	"EXAMPLE: 
	This example draws an oval with a red border and overlays the approximating bezier segments on top of the oval (drawn in black), thus giving an impression of how closely the bezier resembles the oval. Change the rectangle to see how accurate the approximation is for various radii of the oval.
		| rect |
		rect := 100@100 extent: 1200@500.
		Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
		(Bezier2Segment makeEllipseSegments: rect) do:[:seg|
			seg lineSegmentsDo:[:last :next|
				Display getCanvas line: last to: next width: 1 color: Color black]].
	"
	"EXAMPLE: 
		| minRadius maxRadius |
		maxRadius := 300.
		minRadius := 20.
		maxRadius to: minRadius by: -10 do:[:rad|
			| rect |
			rect := 400@400 - rad corner: 400@400 + rad.
			Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
			(Bezier2Segment makeEllipseSegments: rect) do:[:seg|
				seg lineSegmentsDo:[:last :next|
					Display getCanvas line: last to: next width: 1 color: Color black]]].
	"
	| nrm topCenter leftCenter rightCenter bottomCenter dir scale seg1a topRight seg1b seg2a bottomRight seg2b center bottomLeft topLeft seg3a seg3b seg4a seg4b |
	dir := aRectangle width * 0.5.
	nrm := aRectangle height * 0.5.

	"Compute the eight control points on the oval"
	scale := 0.7071067811865475. "45 degreesToRadians cos = 45 degreesToRadians sin = 2 sqrt / 2"
	center := aRectangle origin + aRectangle corner * 0.5.

	topCenter := aRectangle topCenter.
	rightCenter := aRectangle rightCenter.
	leftCenter := aRectangle leftCenter.
	bottomCenter := aRectangle bottomCenter.

	topRight := (center x + (dir * scale)) @ (center y - (nrm * scale)).
	bottomRight := (center x + (dir * scale)) @ (center y + (nrm * scale)).
	bottomLeft := (center x - (dir * scale)) @ (center y + (nrm * scale)).
	topLeft := (center x - (dir * scale)) @ (center y - (nrm * scale)).

	scale := 0.414213562373095. "2 sqrt - 1"

	dir := (dir * scale) @ 0.
	nrm := 0 @ (nrm * scale).
	
	seg1a := self from: topCenter via: topCenter + dir to: topRight.
	seg1b := self from: topRight via: rightCenter - nrm to: rightCenter.

	seg2a := self from: rightCenter via: rightCenter + nrm to: bottomRight.
	seg2b := self from: bottomRight via: bottomCenter + dir to: bottomCenter.

	seg3a := self from: bottomCenter via: bottomCenter - dir to: bottomLeft.
	seg3b := self from: bottomLeft via: leftCenter + nrm to: leftCenter.

	seg4a := self from: leftCenter via: leftCenter - nrm to: topLeft.
	seg4b := self from: topLeft via: topCenter - dir to: topCenter.

	^{seg1a. seg1b. seg2a. seg2b. seg3a. seg3b. seg4a. seg4b}
]

{ #category : #converting }
Bezier2Segment >> asBezier2Points: error [
	^Array with: start with: via with: end
]

{ #category : #converting }
Bezier2Segment >> asBezier2Segment [
	"Represent the receiver as quadratic bezier segment"
	^self
]

{ #category : #converting }
Bezier2Segment >> asBezier3Segment [
	"Represent the receiver as cubic bezier segment"
	^Bezier3Segment
		from: start
		via: 2*via+start / 3.0
		and: 2*via+end / 3.0
		to: end
]

{ #category : #converting }
Bezier2Segment >> asIntegerSegment [
	"Convert the receiver into integer representation"
	^self species 
			from: start asIntegerPoint 
			to: end asIntegerPoint 
			via: via asIntegerPoint
]

{ #category : #converting }
Bezier2Segment >> asTangentSegment [
	^LineSegment from: via-start to: end-via
]

{ #category : #'bezier clipping' }
Bezier2Segment >> bezierClipHeight: dir [
	| dirX dirY uMin uMax dx dy u |
	dirX := dir x.
	dirY := dir y.
	uMin := 0.0.
	uMax := (dirX * dirX) + (dirY * dirY).
	dx := via x - start x.
	dy := via y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].
	^uMin@uMax
]

{ #category : #accessing }
Bezier2Segment >> bounds [
	"Return the bounds containing the receiver"
	^super bounds encompass: via
]

{ #category : #'vector functions' }
Bezier2Segment >> controlPoints [
	^{start. via. end}
]

{ #category : #'vector functions' }
Bezier2Segment >> controlPointsDo: aBlock [
	aBlock value: start; value: via; value: end
]

{ #category : #'vector functions' }
Bezier2Segment >> curveFrom: param1 to: param2 [
	"Return a new curve from param1 to param2"
	| newStart newEnd newVia tan1 tan2 d1 d2 |
	tan1 := via - start.
	tan2 := end - via.
	param1 <= 0.0 ifTrue:[
		newStart := start.
	] ifFalse:[
		d1 := tan1 * param1 + start.
		d2 := tan2 * param1 + via.
		newStart := (d2 - d1) * param1 + d1
	].
	param2 >= 1.0 ifTrue:[
		newEnd := end.
	] ifFalse:[
		d1 := tan1 * param2 + start.
		d2 := tan2 * param2 + via.
		newEnd := (d2 - d1) * param2 + d1.
	].
	tan2 := (tan2 - tan1 * param1 + tan1) * (param2 - param1).
	newVia := newStart + tan2.
	^self shallowCopy from: newStart to: newEnd via: newVia.
]

{ #category : #accessing }
Bezier2Segment >> degree [
	^2
]

{ #category : #initialize }
Bezier2Segment >> from: startPoint to: endPoint [
	"Initialize the receiver as straight line"
	start := startPoint.
	end := endPoint.
	via := (start + end) // 2.
]

{ #category : #initialize }
Bezier2Segment >> from: startPoint to: endPoint via: viaPoint [
	"Initialize the receiver"
	start := startPoint.
	end := endPoint.
	via := viaPoint.
]

{ #category : #initialize }
Bezier2Segment >> from: startPoint to: endPoint withMidPoint: pointOnCurve [
	"Initialize the receiver with the pointOnCurve assumed at the parametric value 0.5"
	start := startPoint.
	end := endPoint.
	"Compute via"
	via := (pointOnCurve * 2) - (start + end * 0.5).
]

{ #category : #initialize }
Bezier2Segment >> from: startPoint to: endPoint withMidPoint: pointOnCurve at: parameter [
	"Initialize the receiver with the pointOnCurve at the given parametric value"
	| t1 t2 t3 |
	start := startPoint.
	end := endPoint.
	"Compute via"
	t1 := (1.0 - parameter) squared.
	t2 := 1.0 / (2 * parameter * (1.0 - parameter)).
	t3 := parameter squared.
	via := (pointOnCurve - (start * t1)  - (end * t3)) * t2
]

{ #category : #testing }
Bezier2Segment >> hasZeroLength [
	"Return true if the receiver has zero length"
	^start = end and:[start = via]
]

{ #category : #initialize }
Bezier2Segment >> initializeFrom: controlPoints [
	controlPoints size = 3 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	via := controlPoints at: 2.
	end := controlPoints at: 3.
]

{ #category : #testing }
Bezier2Segment >> isBezier2Segment [
	"Return true if the receiver is a quadratic bezier segment"
	^true
]

{ #category : #testing }
Bezier2Segment >> isStraight [
	"Return true if the receiver represents a straight line"
	^(self tangentAtStart crossProduct: self tangentAtEnd) = 0
]

{ #category : #'vector functions' }
Bezier2Segment >> length [
	"Return the length of the receiver"
	"Note: Overestimates the length"
	^(start distanceTo: via) + (via distanceTo: end)
]

{ #category : #'vector functions' }
Bezier2Segment >> lineSegments: steps do: aBlock [
	"Evaluate aBlock with the receiver's line segments"
	"Note: We could use forward differencing here."
	| last deltaStep t next |
	last := start.
	deltaStep := 1.0 / steps asFloat.
	t := deltaStep.
	1 to: steps do:[:i|
		next := self valueAt: t.
		aBlock value: last value: next.
		last := next.
		t := t + deltaStep].
]

{ #category : #'vector functions' }
Bezier2Segment >> lineSegmentsDo: aBlock [
	"Evaluate aBlock with the receiver's line segments"
	"Note: We could use forward differencing here."
	| steps last deltaStep t next |
	steps := 1 max: (self length // 10). "Assume 10 pixels per step"
	last := start.
	deltaStep := 1.0 / steps asFloat.
	t := deltaStep.
	1 to: steps do:[:i|
		next := self valueAt: t.
		aBlock value: last value: next.
		last := next.
		t := t + deltaStep].
]

{ #category : #'vector functions' }
Bezier2Segment >> outlineSegment: width [
	| delta newStart newEnd param newMid |
	delta := self tangentAtStart normalized * width.
	delta := delta y @ delta x negated.
	newStart := start + delta.
	delta := self tangentAtEnd normalized * width.
	delta := delta y @ delta x negated.
	newEnd := end + delta.
	param := 0.5. "self tangentAtStart r / (self tangentAtStart r + self tangentAtEnd r)."
	delta := (self tangentAt: param) normalized * width.
	delta := delta y @ delta x negated.
	newMid := (self valueAt: param) + delta.
	^self class from: newStart to: newEnd withMidPoint: newMid at: param
]

{ #category : #'vector functions' }
Bezier2Segment >> parameterAtExtreme: tangentDirection [
	"Compute the parameter value at which the tangent reaches tangentDirection.
	We need to find the parameter value t at which the following holds

		((t * dir + in) crossProduct: tangentDirection) = 0.

	Since this is pretty ugly we use the normal direction rather than the tangent and compute the equivalent relation using the dot product as

		((t * dir + in) dotProduct: nrm) = 0.

	Reformulation yields

		((t * dir x + in x) * nrm x) + ((t * dir y + in y) * nrm y) = 0.
		(t * dir x * nrm x) + (in x * nrm x) + (t * dir y * nrm y) + (in y * nrm y) = 0.
		(t * dir x * nrm x) + (t * dir y * nrm y) = 0 - ((in x * nrm x) + (in y * nrm y)).

				(in x * nrm x) + (in y * nrm y)
		t = 0 -	---------------------------------------
			 	(dir x * nrm x) + (dir y * nrm y)
	And that's that. Note that we can get rid of the negation by computing 'dir' the other way around (e.g., in the above it would read '-dir') which is trivial to do. Note also that the above does not generalize easily beyond 2D since its not clear how to express the 'normal direction' of a tangent plane.
	"
	| inX inY dirX dirY nrmX nrmY |
	"Compute in"
	inX := via x - start x.
	inY := via y - start y.
	"Compute -dir"
	dirX := inX - (end x - via x).
	dirY := inY - (end y - via y).
	"Compute nrm"
	nrmX := tangentDirection y.
	nrmY := 0 - tangentDirection x.
	"Compute result"
	^((inX * nrmX) + (inY * nrmY)) / 
		((dirX * nrmX) + (dirY * nrmY))
]

{ #category : #'vector functions' }
Bezier2Segment >> parameterAtExtremeX [
	"Note: Only valid for non-monoton receivers"
	^self parameterAtExtreme: 0.0@1.0.

]

{ #category : #'vector functions' }
Bezier2Segment >> parameterAtExtremeY [
	"Note: Only valid for non-monoton receivers"
	^self parameterAtExtreme: 1.0@0.0.

]

{ #category : #printing }
Bezier2Segment >> printOn: aStream [
	"Print the receiver on aStream"
	aStream 
		nextPutAll: self class name;
		nextPutAll:' from: ';
		print: start;
		nextPutAll: ' via: ';
		print: via;
		nextPutAll: ' to: ';
		print: end;
		space.
]

{ #category : #'vector functions' }
Bezier2Segment >> roundTo: quantum [
	super roundTo: quantum.
	via := via roundTo: quantum.

]

{ #category : #'vector functions' }
Bezier2Segment >> tangentAt: parameter [
	"Return the tangent at the given parametric value along the receiver"
	| in out |
	in := self tangentAtStart.
	out := self tangentAtEnd.
	^in + (out - in * parameter)
]

{ #category : #'vector functions' }
Bezier2Segment >> tangentAtEnd [
	"Return the tangent for the last point"
	^end - via
]

{ #category : #'vector functions' }
Bezier2Segment >> tangentAtMid [
	"Return the tangent at the given parametric value along the receiver"
	| in out |
	in := self tangentAtStart.
	out := self tangentAtEnd.
	^in + out * 0.5
]

{ #category : #'vector functions' }
Bezier2Segment >> tangentAtStart [
	"Return the tangent for the first point"
	^via - start
]

{ #category : #'vector functions' }
Bezier2Segment >> valueAt: parameter [
	"Evaluate the receiver at the given parametric value"
	"Return the point at the parametric value t:
		p(t) =	(1-t)^2 * p1 + 
				2*t*(1-t) * p2 + 
				t^2 * p3.
	"
	| t1 t2 t3 |
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(start * t1) + (via * t2) + (end * t3)
]

{ #category : #accessing }
Bezier2Segment >> via [
	"Return the control point"
	^via
]
