"
This class represents a cubic bezier segment between two points

Instance variables:
	via1, via2	<Point>	The additional control points (OFF the curve)
"
Class {
	#name : #Bezier3Segment,
	#superclass : #LineSegment,
	#instVars : [
		'via1',
		'via2'
	],
	#category : #'Balloon-Geometry'
}

{ #category : #utilities }
Bezier3Segment class >> convertBezier3ToBezier2: vertices [
	| pa pts index c |
	pts := OrderedCollection new.
	1 to: vertices size // 4 do:
		[:i |
		index := i * 4 - 3.
		c := Bezier3Segment new
					from: (vertices at: index)
					via: (vertices at: index + 1)
					and: (vertices at: index + 2)
					to: (vertices at: index + 3).
		pts addAll: c asBezierShape].
	pa := PointArray new: pts size.
	pts withIndexDo: [:p :i | pa at: i put: p ].
	^ pa
]

{ #category : #'instance creation' }
Bezier3Segment class >> from: p1 to: p2 [
	^ self new from: p1 via: (p1 interpolateTo: p2 at: 0.3333) and: (p1
interpolateTo: p2 at: 0.66667) to: p2
]

{ #category : #'instance creation' }
Bezier3Segment class >> from: p1 via: p2 and: p3 to: p4 [
	^ self new from: p1 via: p2 and: p3 to: p4
]

{ #category : #utilities }
Bezier3Segment class >> makeEllipseSegments: aRectangle [
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates four bezier segments (one for each quadrant) approximating the oval."
	"EXAMPLE: 
	This example draws an oval with a red border and overlays the approximating bezier segments on top of the oval (drawn in black), thus giving an impression of how closely the bezier resembles the oval. Change the rectangle to see how accurate the approximation is for various radii of the oval.
		| rect |
		rect := 100@100 extent: 500@200.
		Display getCanvas fillOval: rect color: Color yellow borderWidth: 1 borderColor: Color red.
		(Bezier3Segment makeEllipseSegments: rect) do:[:seg|
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
			(Bezier3Segment makeEllipseSegments: rect) do:[:seg|
				seg lineSegmentsDo:[:last :next|
					Display getCanvas line: last to: next width: 1 color: Color black]]].
	"
	^self makeEllipseSegments: aRectangle count: 4
]

{ #category : #utilities }
Bezier3Segment class >> makeEllipseSegments: aRectangle count: segmentCount [
	"Answer a set of bezier segments approximating an ellipsoid fitting the given rectangle.
	This method creates segmentCount bezier segments (one for each quadrant) approximating the oval."
	| count angle center scale |
	center := aRectangle origin + aRectangle corner * 0.5.
	scale := aRectangle extent * 0.5.
	count := segmentCount max: 2. "need at least two segments"
	angle := 360.0 / count.
	^(1 to: count) collect:[:i| | seg |
		seg := self makeUnitPieSegmentFrom: i-1*angle to: i*angle.
		self controlPoints: (seg controlPoints collect:[:pt| pt * scale + center])
	].
]

{ #category : #utilities }
Bezier3Segment class >> makePieSegment: aRectangle from: angle1 to: angle2 [
	"Create a single pie segment for the oval inscribed in aRectangle between angle1 and angle2. If angle1 is less than angle2 this method creates a CW pie segment, otherwise it creates a CCW pie segment."
	| seg center scale |
	angle1 > angle2 ifTrue:["ccw"
		^(self makePieSegment: aRectangle from: angle2 to: angle1) reversed
	].
	"create a unit circle pie segment from angle1 to angle2"
	seg := self makeUnitPieSegmentFrom: angle1 to: angle2.
	"scale the segment to fit aRectangle"
	center := aRectangle origin + aRectangle corner * 0.5.
	scale := aRectangle extent * 0.5.
	^self controlPoints: (seg controlPoints collect:[:pt| pt * scale + center])
]

{ #category : #utilities }
Bezier3Segment class >> makePieSegments: aRectangle from: angle1 to: angle2 [
	"Create a series of cubic bezier segments for the oval inscribed in aRectangle between angle1 and angle2. The segments are oriented clockwise, to get counter-clockwise segments simply switch angle1 and angle2."
	angle2 < angle1 ifTrue:[
		"ccw segments"
		^(self makePieSegments: aRectangle from: angle2 to: angle1) 
			reversed collect:[:seg| seg reversed]
	].
	"Split up segments if larger than 120 degrees"
	angle2 - angle1 > 120 ifTrue:["subdivide"
		| midAngle |
		midAngle := angle1 + angle2 * 0.5.
		^(self makePieSegments: aRectangle from: angle1 to: midAngle),
			(self makePieSegments: aRectangle from: midAngle to: angle2).
	].
	"Create actual pie segment"
	^self makePieSegment: aRectangle from: angle1 to: angle2

]

{ #category : #utilities }
Bezier3Segment class >> makeUnitPieSegmentFrom: angle1 to: angle2 [
	"Create a clockwise unit pie segment from angle1 to angle2, that is a pie segment for a circle centered at zero with radius one. Note: This method can be used to create at most a quarter circle."
	| pt1 pt2 rad1 rad2 |
	rad1 := angle1 degreesToRadians.
	rad2 := angle2 degreesToRadians.
	pt1 := rad1 sin @ rad1 cos negated.
	pt2 := rad2 sin @ rad2 cos negated.
	^self makeUnitPieSegmentWith: pt1 and: pt2
]

{ #category : #utilities }
Bezier3Segment class >> makeUnitPieSegmentWith: point1 and: point2 [
	"Create a clockwise unit pie segment from point1 to point2, that is a pie segment for a circle centered at zero with radius one."
	| pt1 pt2 dir1 dir2 mid length scale cp1 cp2 pt3 magic |
	"point1 and point2 are the points on the unit circle
	for accuracy (or broken input), renormalize them."
	pt1 := point1 normalized.
	pt2 := point2 normalized.
	"compute the normal vectors - those are tangent directions for the bezier"
	dir1 := pt1 y negated @ pt1 x.
	dir2 := pt2 y negated @ pt2 x.
	"Okay, now that we have the points and tangents on the unit circle, let's do the magic. For fitting a cubic bezier onto a circle section we know that we want the end points be on the circle and the tangents to point towards the right direction (both of which we have in the above). What we do NOT know is how to scale the tangents so that midpoint of the bezier is exactly on the circle.
	The good news is that there is a linear relation between the length of the tangent vectors and the distance of the midpoint from the circle's origin. The bad news is that I don't know how to derive it analytically. So what I do here is simply sampling the bezier twice (not really - the first sample is free) and then to compute the distance from the sample."

	"The first sample is just between the two points on the curve"
	mid := pt1 + pt2 * 0.5.

	"The second sample will be taken from the curve with coincident control points at the intersection of dir1 and dir2, which simplifies significantly with a little understanding about trigonometry, since the angle formed between mid, pt1 and the intersection is the same as between the center, pt1 and mid."
	length := mid r.
	"length is not only the distance from the center of the unit circle but also the sine of the angle between the circle's center, pt1 and mid (since center is at zero and pt1 has unit length). Therefore, to scale dir1 to the intersection with dir2 we can use mid's distance from pt1 and simply divide it by the sine value."
	scale := (mid distanceTo: pt1).
	length > 0.0 ifTrue:[ scale := scale / length].
	"now sample the cubic bezier (optimized version for coincident control points)"
	cp1 := pt1 + (dir1 * (scale * 0.75)).
	cp2 := pt2 - (dir2 * (scale * 0.75)).
	pt3 := cp1 + cp2 * 0.5.
	"compute the magic constant"
	scale := (pt3 - mid) r / scale.
	magic := 1.0 - length / scale.
	"and finally answer the pie segment"
	^self
		from: pt1
		via: pt1 + (dir1 * magic)
		and: pt2 - (dir2 * magic)
		to: pt2
]

{ #category : #converting }
Bezier3Segment >> asBezier2Points: error [
	"Demote a cubic bezier to a set of approximating quadratic beziers.
	Should convert to forward differencing someday"

	| curves pts step prev index a b f |
	curves := self bezier2SegmentCount: error.
	pts := Array new: curves * 3.
	step := 1.0 / (curves * 2).
	prev := start.
	1 to: curves do: [ :c |
		index := 3*c.
		a := pts at: index-2 put: prev.
		b := (self valueAt: (c*2-1)*step).
		f := pts at: index put: (self valueAt: (c*2)*step).
		pts at: index-1 put: (4 * b - a - f) / 2.
		prev := pts at: index.
		].
	^ pts.
	
]

{ #category : #converting }
Bezier3Segment >> asBezier2Segments [
	"Demote a cubic bezier to a set of approximating quadratic beziers."
	^self asBezier2Segments: 0.5
]

{ #category : #converting }
Bezier3Segment >> asBezierShape [
	"Demote a cubic bezier to a set of approximating quadratic beziers."
	^self asBezierShape: 0.5
]

{ #category : #converting }
Bezier3Segment >> asBezierShape: error [
	"Demote a cubic bezier to a set of approximating quadratic beziers.
	Should convert to forward differencing someday"
	^(self asBezier2Points: error) asPointArray.
]

{ #category : #converting }
Bezier3Segment >> asPointArray [
	| p |
	p := PointArray new: 4.
	p at: 1 put: start.
	p at: 2 put: via1.
	p at: 3 put: via2.
	p at: 4 put: end.
	^ p
]

{ #category : #converting }
Bezier3Segment >> asTangentSegment [
	^Bezier2Segment 
		from: via1-start 
		via: via2-via1
		to: end-via2
]

{ #category : #private }
Bezier3Segment >> bezier2SegmentCount [
	"Compute the number of quadratic bezier segments needed to approximate
	this cubic with less than a 1-pixel error"
	^ self bezier2SegmentCount: 1.0
]

{ #category : #converting }
Bezier3Segment >> bezier2SegmentCount: pixelError [
	"Compute the number of quadratic bezier segments needed to approximate
	this cubic with no more than a specified error"
	| a |
	a := (start x negated @ start y negated) + (3 * via1) - (3 * via2) +
(end).
	^ (((a r / (20.0 * pixelError)) raisedTo: 0.333333) ceiling) max: 1.


]

{ #category : #'bezier clipping' }
Bezier3Segment >> bezierClipHeight: dir [
	"Check if the argument overlaps the receiver somewhere 
	along the line from start to end. Optimized for speed."
	| u dirX dirY dx dy uMin uMax |
	dirX := dir x.
	dirY := dir y.
	uMin := 0.0.
	uMax := (dirX * dirX) + (dirY * dirY).

	dx := via1 x - start x.
	dy := via1 y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].

	dx := via2 x - start x.
	dy := via2 y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].

	^uMin@uMax
]

{ #category : #accessing }
Bezier3Segment >> bounds [
	^ ((super bounds encompassing: via1) encompassing: via2)
]

{ #category : #'vector functions' }
Bezier3Segment >> controlPoints [
	^{start. via1. via2. end}
]

{ #category : #'vector functions' }
Bezier3Segment >> controlPointsDo: aBlock [
	aBlock value: start; value: via1; value: via2; value: end
]

{ #category : #accessing }
Bezier3Segment >> degree [
	^3
]

{ #category : #initialization }
Bezier3Segment >> from: aPoint1 via: aPoint2 and: aPoint3 to: aPoint4 [
	start := aPoint1.
	via1 := aPoint2.
	via2 := aPoint3.
	end := aPoint4
]

{ #category : #initialization }
Bezier3Segment >> initializeFrom: controlPoints [
	controlPoints size = 4 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	via1 := controlPoints at: 2.
	via2 := controlPoints at: 3.
	end := controlPoints at: 4.
]

{ #category : #accessing }
Bezier3Segment >> length [
	"Answer a gross approximation of the length of the receiver"
	^(start distanceTo: via1) + (via1 distanceTo: via2) + (via2 distanceTo: end)
]

{ #category : #'vector functions' }
Bezier3Segment >> lineSegments: steps do: aBlock [
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
Bezier3Segment >> lineSegmentsDo: aBlock [
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
Bezier3Segment >> outlineSegment: width [
	| tan1 nrm1 tan2 nrm2 newStart newVia1 newEnd newVia2 dist |
	tan1 := (via1 - start) normalized.
	nrm1 := tan1 * width.
	nrm1 := nrm1 y @ nrm1 x negated.
	tan2 := (end - via2) normalized.
	nrm2 := tan2 * width.
	nrm2 := nrm2 y @ nrm2 x negated.
	newStart := start + nrm1.
	newEnd := end + nrm2.
	dist := (newStart distanceTo: newEnd) * 0.3.
	newVia1 := newStart + (tan1 * dist).
	newVia2 := newEnd - (tan2 * dist).
	^self class from: newStart via: newVia1 and: newVia2 to: newEnd.

]

{ #category : #'vector functions' }
Bezier3Segment >> tangentAt: parameter [
	| tan1 tan2 tan3 t1 t2 t3 |
	tan1 := via1 - start.
	tan2 := via2 - via1.
	tan3 := end - via2.
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(tan1 * t1) + (tan2 * t2) + (tan3 * t3)
]

{ #category : #'vector functions' }
Bezier3Segment >> tangentAtEnd [
	^end - via2
]

{ #category : #'vector functions' }
Bezier3Segment >> tangentAtMid [
	| tan1 tan2 tan3 |
	tan1 := via1 - start.
	tan2 := via2 - via1.
	tan3 := end - via2.
	^(tan1 + (2*tan2) + tan3) * 0.25

]

{ #category : #'vector functions' }
Bezier3Segment >> tangentAtStart [
	^via1 - start
]

{ #category : #accessing }
Bezier3Segment >> valueAt: t [
	| a b c d |

	"| p1 p2 p3 |
	p1 := start interpolateTo: via1 at: t.
	p2 := via1 interpolateTo: via2 at: t.
	p3 := via2 interpolateTo: end at: t.
	p1 := p1 interpolateTo: p2 at: t.
	p2 := p2 interpolateTo: p3 at: t.
	^ p1 interpolateTo: p2 at: t"

	a := (start negated) + (3 * via1) - (3 * via2) + (end).
	b := (3 * start) - (6 * via1) + (3 * via2).
	c := (3 * start negated) + (3 * via1).
	d := start.
	^ ((a * t + b) * t + c) * t + d


]

{ #category : #accessing }
Bezier3Segment >> via1 [
	^via1
]

{ #category : #accessing }
Bezier3Segment >> via1: aPoint [
	via1 := aPoint
]

{ #category : #accessing }
Bezier3Segment >> via2 [
	^via2
]

{ #category : #accessing }
Bezier3Segment >> via2: aPoint [
	via2 := aPoint
]
