"
Cubic bezier with 4 control points encapsulation.

Main purpose of this class is to keep subdivision logic in clean and separate place
"
Class {
	#name : #AthensCubicBezier,
	#superclass : #Object,
	#instVars : [
		'x1',
		'y1',
		'x2',
		'y2',
		'x3',
		'y3',
		'x4',
		'y4'
	],
	#category : #'Athens-Core-PathsGeometry'
}

{ #category : #subdivision }
AthensCubicBezier >> recursiveSubDiv: flattener level: level [

	| dx dy d2 d3 da1 da2 k f1 f2 |


	" Try to approximate the full cubic curve by a single straight line "
	level > flattener subdivisionLimit ifTrue: [  ^ self ].

	dx := x4-x1.
	dy := y4-y1.

	d2 := ((x2 - x4) * dy - ((y2 - y4) * dx)) abs.
	d3 := ((x3 - x4) * dy - ((y3 - y4) * dx)) abs.

	f1 := d2 > flattener curveCollinearityEpsilon.
	f2 := d3 > flattener curveCollinearityEpsilon.


	f1 ifTrue: [
		f2 ifTrue: [
			" Regular case "

			(d2 + d3) squared <= (flattener distanceToleranceSquared * (dx squared + dy squared)) ifTrue: [
				"If the curvature doesn't exceed the distance_tolerance value
				we tend to finish subdivisions."
			"	flattener accountForAngleTolerance ifFalse: [
					^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5) y: (y2 interpolateTo: y3 at: 0.5)
					].
			"
				"Angle & Cusp Condition"

				k := (y3-y2) arcTan: (x3 - x2).
				da1 := (k - ((y2-y1) arcTan: (x2-x1))) abs.
				da2 := ((y4-y3 arcTan: x4-x3) - k) abs.

				da1 >= Float pi ifTrue: [  da1 := Float pi*2 - da1 ].
				da2 >= Float pi ifTrue: [  da2 := Float pi*2 - da2 ].

				(da1 + da2) < flattener angleTolerance ifTrue: [
					" Finally we can stop the recursion "
					^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5) y: (y2 interpolateTo: y3 at: 0.5)
					 ].

				(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x2 y: y2 ].
            	   	(flattener overCuspLimit: da2) ifTrue: [ ^ flattener lineToX: x3 y: y3 ].
			]

		] ifFalse: [

			"p1,p3,p4 are collinear, p2 is significant"

			(d2 squared <= (flattener distanceToleranceSquared * (dx squared + dy squared) )  )
				ifTrue: [
					flattener accountForAngleTolerance ifFalse: [
						^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5)  y: (y2 interpolateTo: y3 at: 0.5) ].

					"Angle Condition"
					da1 := ((y3-y2 arcTan: (x3-x2)) - (y2-y1 arcTan:(x2-x1))) abs.
					da1 >= Float pi ifTrue: [  da1 := Float pi * 2 - da1 ].

					da1 < flattener angleTolerance ifTrue: [
						^ flattener
							lineToX: x2 y: y2;
							lineToX: x3 y: y3 ].

					(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x2 y: y2 ]
				]
			 ]
	] ifFalse: [
		f2 ifTrue: [
		"p1,p2,p4 are collinear, p3 is significant "

			(d3 squared <= (flattener distanceToleranceSquared * (dx squared + dy squared) )  )
				ifTrue: [
					flattener accountForAngleTolerance ifFalse: [
						^ flattener lineToX: (x2 interpolateTo: x3 at: 0.5)  y: (y2 interpolateTo: y3 at: 0.5) ].

					"Angle Condition"
					da1 := ((y4-y3 arcTan: (x4-x3)) - (y3-y2 arcTan:(x3-x2))) abs.
					da1 >= Float pi ifTrue: [  da1 := Float pi * 2 - da1 ].

					da1 < flattener angleTolerance ifTrue: [
						^ flattener
							lineToX: x2 y: y2;
							lineToX: x3 y: y3 ].

					(flattener overCuspLimit: da1) ifTrue: [ ^ flattener lineToX: x3 y: y3 ]
				]
			  ] ifFalse: [
		"All collinear OR p1==p4 "
			k := dx*dx + (dy*dy).

			(k = 0.0) ifTrue: [
				d2 := (x1-x2) squared + (y1-y2) squared.
				d3 := (x3-x4) squared + (y3-y4) squared ]
			ifFalse: [

				k := 1 / k.
				da1 := x2 - x1.
				da2 := y2 - y1.
				d2  := k * (da1*dx + (da2*dy)).
				da1 := x3 - x1.
				da2 := y3 - y1.
				d3  := k * (da1*dx + (da2*dy)).

				(d2 > 0.0 and: [ d2 < 1.0 and: [d3>0.0 and: [d3 < 1.0]]]) ifTrue: [
						" Simple collinear case, 1---2---3---4  We can leave just two endpoints"
							^ self ].
				d2 <= 0.0
					ifTrue: [ d2 := (x1-x2) squared + (y1-y2) squared ]
					ifFalse: [
						d2 >= 1.0
							ifTrue: [ d2 := (x2-x4) squared + (y2-y4) squared ]
							ifFalse: [ d2 := (x2 - x1 - (d2*dx)) squared + (y2 - y1 - (d2*dy)) squared ]].

				d3 <= 0.0
					ifTrue: [  d3 := (x3-x1) squared + (y3-y1) squared ]
					ifFalse: [
						d3 >= 1.0
							ifTrue: [ d3 := (x3-x4) squared + (y3-y4) squared   ]
							ifFalse: [ d3 := (x3 - x1 - (d3*dx)) squared + (y3-y1- (d3*dy)) squared ]].
			].

			(d2 > d3) ifTrue: [
				(d2 < flattener distanceToleranceSquared)
					ifTrue: [ ^ flattener lineToX: x2 y: y2 ]
				]
			ifFalse: [
				(d3 < flattener distanceToleranceSquared )
					ifTrue: [  ^ flattener lineToX: x3 y: y3 ]
				 ].
		]
	].
	self subdivideAt: 0.5 do:  [  :b1 :b2 |
		b1 recursiveSubDiv: flattener level: level +1.
		b2 recursiveSubDiv: flattener level: level +1.
	]
]

{ #category : #subdivision }
AthensCubicBezier >> subdivideAt: t do: aBinaryBlock [
	| x12 y12 x23 y23 x34 y34 x1223 y1223 x2334 y2334 xsplit ysplit |

	x12 := x1 interpolateTo: x2 at: t.
	y12 := y1 interpolateTo: y2 at: t.
	x23 := x2 interpolateTo: x3 at: t.
	y23 := y2 interpolateTo: y3 at: t.
	x34 := x3 interpolateTo: x4 at: t.
	y34 := y3 interpolateTo: y4 at: t.

	x1223 := x12 interpolateTo: x23 at: t.
	x2334 := x23 interpolateTo: x34 at: t.
	y1223 := y12 interpolateTo: y23 at: t.
	y2334 := y23 interpolateTo: y34 at: t.

	xsplit := x1223 interpolateTo: x2334 at: t.
	ysplit := y1223 interpolateTo: y2334 at: t.

	^ aBinaryBlock
		value:
		"p1 - p12 - p1223 - psplit"
		(self copy
			x2: x12;
			y2: y12;
			x3: x1223;
			y3: y1223;
			x4: xsplit;
			y4: ysplit )
		value:
		"psplit - p2334 - p34 - p4"
		(self copy
			x1: xsplit;
			y1: ysplit;
			x2: x2334;
			y2: y2334;
			x3: x34;
			y3: y34
		)
]

{ #category : #accessing }
AthensCubicBezier >> x1 [

	^ x1
]

{ #category : #accessing }
AthensCubicBezier >> x1: anObject [

	x1 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> x2 [

	^ x2
]

{ #category : #accessing }
AthensCubicBezier >> x2: anObject [

	x2 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> x3 [

	^ x3
]

{ #category : #accessing }
AthensCubicBezier >> x3: anObject [

	x3 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> x4 [

	^ x4
]

{ #category : #accessing }
AthensCubicBezier >> x4: anObject [

	x4 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> y1 [

	^ y1
]

{ #category : #accessing }
AthensCubicBezier >> y1: anObject [

	y1 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> y2 [

	^ y2
]

{ #category : #accessing }
AthensCubicBezier >> y2: anObject [

	y2 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> y3 [

	^ y3
]

{ #category : #accessing }
AthensCubicBezier >> y3: anObject [

	y3 := anObject
]

{ #category : #accessing }
AthensCubicBezier >> y4 [

	^ y4
]

{ #category : #accessing }
AthensCubicBezier >> y4: anObject [

	y4 := anObject
]
