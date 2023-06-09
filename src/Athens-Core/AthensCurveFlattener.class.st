"
This converter flattens all curves by converting them to series of lines.
This means that path can be rendered using simple polygon rendering technique
and nothing else.
"
Class {
	#name : #AthensCurveFlattener,
	#superclass : #AthensPathSegmentConverter,
	#instVars : [
		'transform',
		'lengthTolerance',
		'lengthToleranceSquared'
	],
	#classVars : [
		'CurveAngleToleranceEpsilon',
		'CurveCollinearityEpsilon',
		'CurveDistanceEpsilon',
		'SubdivisionLimit'
	],
	#category : #'Athens-Core-PathsGeometry'
}

{ #category : #'class initialization' }
AthensCurveFlattener class >> initialize [
	CurveCollinearityEpsilon := 1e-30 asFloat.
	CurveDistanceEpsilon := 1e-30 asFloat.
	CurveAngleToleranceEpsilon := 0.01.
	SubdivisionLimit := 10
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> accountForAngleTolerance [
	" It's important only when we want to draw an equidistant curve, that is, a stroke of considerable width. If we don't need to draw a stroke or the stroke width is one pixel or less, the distance criterion works quite well. "
	^ false
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> angleTolerance [
	^ CurveAngleToleranceEpsilon
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> curveCollinearityEpsilon [

	^ CurveCollinearityEpsilon
]

{ #category : #'path commands' }
AthensCurveFlattener >> curveVia: via1 and: via2 to: end [
	| pt1 pt2 pt3 pt4 curve |

	pt1 := transform transform: endPoint.
	pt2 := transform transform: via1.
	pt3 := transform transform: via2.
	pt4 := transform transform: end.

	endPoint := end.

	curve := AthensCubicBezier new
		x1: pt1 x;
		y1: pt1 y;
		x2: pt2 x;
		y2: pt2 y;
		x3: pt3 x;
		y3: pt3 y;
		x4: pt4 x;
		y4: pt4 y.

	curve recursiveSubDiv: self level: 0.
	self lineTo: endPoint
]

{ #category : #'path commands' }
AthensCurveFlattener >> curveVia: via to: end [
	| pt1 pt2 pt3 |

	pt1 := transform transform: endPoint.
	pt2 := transform transform: via.
	pt3 := transform transform: end.

	endPoint := end.

	self flattenQuadBezier: pt1 x y1: pt1 y x2: pt2 x y2: pt2 y x3: pt3 x y3: pt3 y
]

{ #category : #accessing }
AthensCurveFlattener >> defaultTolerance [
	^ 1
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> distanceToleranceSquared [
	^ self lengthToleranceSquared
]

{ #category : #helpers }
AthensCurveFlattener >> flattenCubicBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3 x4: x4 y4: y4 [
]

{ #category : #'public API' }
AthensCurveFlattener >> flattenPath: aPath transform: aTransformation [

	"This is an entry point for flattening path.
	An additional argument, transform is an Affine matrix, used to map path geometry to screen,
	and therefore used to calculate the error tolerance for path subdivisions"

	^ self flattenPath: aPath transform: aTransformation toleranceMultiplier: 1
]

{ #category : #'public API' }
AthensCurveFlattener >> flattenPath: aPath transform: aTransform toleranceMultiplier: aToleranceMultiplier [

	"This is an entry point for flattening path (converting all curves into line segments by approximating them).

	- transform is an Affine matrix, used to map path geometry to screen,
	and therefore used to calculate the error tolerance for path subdivisions


	- a tolerance multiplier is a number .. which
		if = 1 , the default error tolerance is used,
		if > 1 you will get more coarse approximation
		if < 1 you will get more finer approximation

	"

	transform := aTransform.
	lengthTolerance := aToleranceMultiplier.
	lengthToleranceSquared := lengthTolerance squared
]

{ #category : #helpers }
AthensCurveFlattener >> flattenQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3 [

	"Recursively subdive quadric bezier curve as long as #isFlatBezier.. answers false "

	"The points here is unboxed intentionally to avoid generating extra garbage
	(which contributes to performance loss)"

	(self isFlatQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3) ifTrue: [
		| midx midy |
		midx := (x2 + x1 + x2+x3) * 0.25 .  "mid ( mid(pt1,pt2), mid(pt2,pt3)) "
		midy := (y2 + y1 + y2+y3) * 0.25 .

		dest
			lineTo: (self inverseTransform: midx @ midy);
			lineTo: (self inverseTransform: x3 @ y3)

	"	dest
			lineTo: (self inverseTransform: x2 @ y2);
			lineTo: (self inverseTransform: x3 @ y3)
	"

	] ifFalse: [
		| x12 y12 x23 y23 x123 y123 |
	"calculate midpoints of line segments "
		x12 := (x1 + x2) * 0.5.
		y12 := (y1 + y2) * 0.5 .

		x23 := (x2 + x3) * 0.5 .
		y23 := (y2 + y3) * 0.5 .

		x123 := (x12 + x23) * 0.5.
		y123 := (y12 + y23) * 0.5.

		self flattenQuadBezier: x1 y1: y1
			x2: x12
			y2: y12
			x3: x123
			y3: y123.

		self flattenQuadBezier: x123
			y1: y123
			x2: x23
			y2: y23
			x3: x3
			y3: y3.
	]
]

{ #category : #initialization }
AthensCurveFlattener >> initialize [

	super initialize.

	transform := AthensAffineTransform new. "identity"
	self toleranceMultiplier: self defaultTolerance
]

{ #category : #helpers }
AthensCurveFlattener >> inverseTransform: aPoint [
	^ transform inverseTransform: aPoint
]

{ #category : #helpers }
AthensCurveFlattener >> isFlatQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3 [

	| dx dy d da |

	dx := x3-x1.
	dy := y3-y1.

	"This is the area of triangle enclosing curve * 2"
 	d := (((x2 - x3) * dy) - ((y2 - y3) * dx)) abs.

	d > (self lengthToleranceSquared ) ifTrue: [

		"Non-collinear case (regular one)"
		| dot |

		"if dot product is close to zero, that means we having flat curve"
		dot := ( (x2-x1)*(x2-x3) + ((y1-y2)*(y2-y3)) ) abs.
		dot < (self lengthToleranceSquared * 0.5 ) ifTrue: [  ^ true ].
	]
	ifFalse: [
		"collinear"
		da := dx*dx + (dy*dy).

		da = 0 "end points coincide"
			ifTrue: [ d := (x1-x2) squared + (y1-y2) squared  "pointy case" ]
			ifFalse: [
				"the control point lies on line between endpoints?"
				d := ((x2 - x1)*dx + ((y2 - y1)*dy)) / da.

				(d > 0.0 and: [ d < 1.0 ] ) ifTrue: [
					"Simple collinear case, 1---2---3"
					^ true
     				].
				d <= 0.0
					ifTrue: [ d := (x1-x2) squared + (y1-y2) squared ]
					ifFalse: [
						d >= 1.0
							ifTrue: [ d:= (x2-x3) squared + (y2-y3) squared ]
							ifFalse: [ d:= (x2 - x1 - (d*dx)) squared + (y2 - y1 - (d*dy)) squared ]
					].
			].

			d < (self lengthToleranceSquared) ifTrue: [ ^ true ]
	].

	^ false
]

{ #category : #accessing }
AthensCurveFlattener >> lengthTolerance [

	^ lengthTolerance
]

{ #category : #accessing }
AthensCurveFlattener >> lengthToleranceSquared [

	^ lengthToleranceSquared
]

{ #category : #'path commands' }
AthensCurveFlattener >> lineTo: aPoint [

	endPoint := aPoint.

	^ dest lineTo: aPoint
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> lineToX: trX y: trY [

	^ dest lineTo: (self inverseTransform: trX @ trY)
]

{ #category : #'path commands' }
AthensCurveFlattener >> moveTo: aPoint [

	contourStartPt := endPoint := aPoint.

	^ dest moveTo: aPoint
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> overCuspLimit: angleInRadians [
	 "if(m_cusp_limit != 0.0)
                {
                    if(da1 > m_cusp_limit)
                    {

	^ angleInRadians > 1.01"
	^ false
]

{ #category : #visiting }
AthensCurveFlattener >> quadricBezierSegment: segment [
	| pt1 pt2 pt3 |

	pt1 := transform transform: endPoint.
	pt2 := transform transform: segment via.
	pt3 := transform transform: segment endPoint.

	endPoint := segment endPoint.

	self flattenQuadBezier: pt1 x y1: pt1 y x2: pt2 x y2: pt2 y x3: pt3 x y3: pt3 y
]

{ #category : #'subdivision callbacks' }
AthensCurveFlattener >> subdivisionLimit [
	"max number of recursive subdivisions for single curve"
	^ SubdivisionLimit
]

{ #category : #accessing }
AthensCurveFlattener >> toleranceMultiplier: aToleranceMultiplier [

	"
	A tolerance multiplier is a number .. which
		if = 1 , the default error tolerance is used,
		if > 1 you will get more coarse approximation
		if < 1 you will get more finer approximation

	"

	lengthTolerance := aToleranceMultiplier.
	lengthToleranceSquared := lengthTolerance squared
]

{ #category : #accessing }
AthensCurveFlattener >> transform: aTransform [
	"
	- transform is an Affine matrix, used to map path geometry to screen,
	and therefore used to calculate the error tolerance for path subdivisions


	- a tolerance multiplier is a number .. which
		if = 1 , the default error tolerance is used,
		if > 1 you will get more coarse approximation
		if < 1 you will get more finer approximation

	"

	transform := aTransform
]
