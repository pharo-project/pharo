"
I just a helper class used to debug & visualize rendering of bezier segment. This class is subject of changes or complete removal
"
Class {
	#name : #AthensBezier3Scene,
	#superclass : #Object,
	#instVars : [
		'pt1',
		'pt2',
		'pt3',
		'pt4'
	],
	#category : #'Athens-Core-PathsGeometry'
}

{ #category : #'instance creation' }
AthensBezier3Scene class >> on: anControlPointArray [

	^ self basicNew
		initFromArray: anControlPointArray
]

{ #category : #accessing }
AthensBezier3Scene >> curvePathBlock [
	^ [ :builder |
		builder
			absolute;
			moveTo: pt1;
			curveVia: pt2 and: pt3 to: pt4
	 ]
]

{ #category : #initialization }
AthensBezier3Scene >> initFromArray: aControlPointsArray [

		pt1 := aControlPointsArray at: 1.
		pt2 := aControlPointsArray at: 2.
		pt3 := aControlPointsArray at: 3.
		pt4 := aControlPointsArray at: 4
]

{ #category : #rendering }
AthensBezier3Scene >> renderOn: canvas [

	| path pathBlock |

	canvas surface clear:  Color black.
	pathBlock :=  [ :builder |
		builder
			absolute;
			moveTo: pt1;
			curveVia: pt2 and: pt3 to: pt4
	 ].


	"First , we draw the curve using Cairo"
	(canvas setStrokePaint: Color green).
	canvas drawShape: (canvas createPath: pathBlock).

	"draw a polygon, connecting control points"
	(canvas setStrokePaint: Color blue) width: 0.5.
	canvas drawShape: (canvas createPath: [ :builder |
		builder
			absolute;
			moveTo: pt1;
			lineTo: pt2;
			lineTo: pt3;
			lineTo: pt4
		 ]).



	canvas setPaint: Color red.

	path := AthensSimplePathBuilder createPath: pathBlock.
"	polygonizedPath := path asPolygon: canvas pathTransform. "

	canvas drawShape: (
		canvas createPath: [ :builder | | flattener |
			builder absolute.

		flattener := AthensCurveFlattener new dest: builder.
		flattener transform: (canvas pathTransform).
		flattener convert:path.
	])
]
