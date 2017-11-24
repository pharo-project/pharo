"
An example from a workshop
"
Class {
	#name : #CurveWorkshop,
	#superclass : #Object,
	#instVars : [
		'surface'
	],
	#category : #'Athens-Examples'
}

{ #category : #colors }
CurveWorkshop >> curveColor [
	^ Color red
]

{ #category : #drawing }
CurveWorkshop >> draw [

	
]

{ #category : #drawing }
CurveWorkshop >> drawOn: canvas [

	

	self drawQuadFrom: 10@200 via: 50@50 to: 200@200 on: canvas
]

{ #category : #drawing }
CurveWorkshop >> drawQuadFrom: pt1 via: pt2 to: pt3 on: canvas [
	| curve triangle middleRay curveBlock |

	curveBlock := [ :builder | 
		builder 
			absolute;
			moveTo: pt1;
			curveVia: pt2 to: pt3
		 ].

	curve := canvas createPath: curveBlock.

	canvas setStrokePaint: self curveColor.
	canvas drawShape: curve.
	
	triangle :=  canvas createPath: [ :builder | 
		builder 
			absolute;
			moveTo: pt1;
			lineTo: pt2;
			lineTo: pt3;
			lineTo: pt1.
	].
	canvas setStrokePaint: self triangleColor.
	canvas drawShape: triangle.

	middleRay :=  canvas createPath: [ :builder | 
		builder 
			absolute;
			moveTo: pt2;
			lineTo: (pt1 + pt3)/2
	].

	canvas setStrokePaint: self middleRayColor.
	canvas drawShape: middleRay.

	triangle := AthensSimplePathBuilder createPath: curveBlock.
	
	triangle := canvas surface createPath: [ :builder | 
		AthensCurveFlattener new
			dest: builder;
		flattenPath: triangle transform: (canvas pathTransform).
	].
	
	canvas 
		setStrokePaint: Color yellow;
		drawShape: triangle.
	

]

{ #category : #colors }
CurveWorkshop >> middleRayColor [
	^ Color blue
]

{ #category : #colors }
CurveWorkshop >> triangleColor [
	^ Color green
]
