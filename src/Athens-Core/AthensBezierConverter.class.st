"
I converting all bezier segments to a sequence of line segments, by approximating the curve along its path.

This classs is used for generating strokes.
"
Class {
	#name : #AthensBezierConverter,
	#superclass : #AthensPathConverter,
	#instVars : [
		'distanceTolerance',
		'angleTolerance'
	],
	#classVars : [
		'CollinearityEps',
		'CurveAngleTolerance',
		'DistanceEps'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #'class initialization' }
AthensBezierConverter class >> initialize [

	CollinearityEps := 1e-30.
	DistanceEps := 1e-30.
	CurveAngleTolerance := 0.01
]

{ #category : #helpers }
AthensBezierConverter >> angleBetween: p1 and: p2 ifDegenerate: aBlock [
	"Calculate an angle (in radians) between two vectors.
	 Evaluate a block, in case if calculation not possible because one of the vectors has zero length"

	| x1 y1 x2 y2 dot2 n2 |
	x1 := p1 x.
	y1 := p1 y.
	x2 := p2 x.
	y2 := p2 y.

	dot2 := x1 * x2 + (y1 * y2).
	dot2 := dot2 * dot2.

	n2 := (x1*x1 + (y1*y1)) * (x2*x2 + (y2*y2)).

	n2 = 0 ifTrue: [ ^ aBlock value ].

	^ (dot2 / n2) arcCos
]

{ #category : #'converting path commands' }
AthensBezierConverter >> curveVia: pt1 to: pt2 [

	self recursiveBezier2_x1: endPoint x y1: endPoint y
		x2: pt1 x y2: pt1 y
		x3: pt2 x y3: pt2 y
]

{ #category : #initialization }
AthensBezierConverter >> initialize [
	super initialize.
	distanceTolerance := 0.5.
	angleTolerance := 0.1
]

{ #category : #testing }
AthensBezierConverter >> isFlatBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3 [

	| dx dy d da angle |

	dx := x3-x1.
	dy := y3-y1.

 	d := (((x2 - x3) * dy) - ((y2 - y3) * dx)) abs.

	d > CollinearityEps ifTrue: [

		"regular case"

		d*d <= (distanceTolerance * ( dx*dx + (dy*dy))) ifTrue: [

			angleTolerance < CurveAngleTolerance ifTrue: [ ^ true ].

			angle := self angleBetween: x2-x1 @ (y2-y1) and: x3-x2 @ (y3-y2)
				ifDegenerate: [ 0.0 ].

			"parallel. no need to proceed further"
			angle <= angleTolerance ifTrue: [ ^ true ]
		]
	]
	ifFalse: [
		"collinear"
		da := dx*dx + (dy*dy).

		da = 0
			ifTrue: [ d := (x1-x2) squared + (y1-y2) squared ]
			ifFalse: [
				d = ((x2 - x1)*dx + ((y2 - y1)*dy)) / da.

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

			d < self distanceToleranceSquared ifTrue: [ ^ true ]
	].

	^ false
]

{ #category : #helpers }
AthensBezierConverter >> recursiveBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3 [

	"recursively subdive bezier curve as long as #isFlatBezier2.. answers false "

	(self isFlatBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3) ifTrue: [

		dest
			lineTo: x2 @ y2;
			lineTo: x3 @ y3
	] ifFalse: [
		| x12 y12 x23 y23 x123 y123 |
	"calculate midpoints of line segments "
		x12 := (x1 + x2) * 0.5.
		y12 := (y1 + y2) * 0.5 .

		x23 := (x2 + x3) * 0.5 .
		y23 := (y2 + y3) * 0.5 .

		x123 := (x12 + x23) * 0.5.
		y123 := (y12 + y23) * 0.5.

		self recursiveBezier2_x1: x1 y1: y1
			x2: x12
			y2: y12
			x3: x123
			y3: y123.

		self recursiveBezier2_x1: x123
			y1: y123
			x2: x23
			y2: y23
			x3: x3
			y3: y3.
	]
]
