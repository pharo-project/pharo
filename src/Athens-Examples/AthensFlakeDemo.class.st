"
AthensFlakeDemo new openInWindow

"
Class {
	#name : #AthensFlakeDemo,
	#superclass : #Morph,
	#instVars : [
		'surface',
		'circle',
		'frame',
		'spike'
	],
	#category : #'Athens-Examples-Morphic'
}

{ #category : #'instance creation' }
AthensFlakeDemo class >> open [
	<example>
	self new openInWindow
]

{ #category : #accessing }
AthensFlakeDemo >> circle [
	^ circle
]

{ #category : #defaults }
AthensFlakeDemo >> defaultExtent [
	^ 800@800
]

{ #category : #drawing }
AthensFlakeDemo >> drawCircles: canvas [

	canvas pathTransform restoreAfter: [

		canvas pathTransform scaleBy: 0.5 .

		canvas
			setPaint: Color black;
			setShape: self circle;
			draw.

		canvas
			pathTransform scaleBy: 0.8.
		canvas
			setPaint: Color white;
			draw.
		canvas
			pathTransform scaleBy: 0.8.
		canvas
			setPaint: Color black;
			draw
	]
]

{ #category : #drawing }
AthensFlakeDemo >> drawOn: aCanvas [

	self render.
	surface displayOnMorphicCanvas: aCanvas at: bounds origin
]

{ #category : #drawing }
AthensFlakeDemo >> drawSpike: canvas [


	|  dist |


	dist := (canvas pathTransform transform: 1@0) - (canvas pathTransform transform: 0@0).
  	dist r < 2 ifTrue: [ ^ self ].

	canvas
		setPaint: (Color black alpha: 0.5);
		drawShape: spike.

	canvas pathTransform restoreAfter: [
 		canvas pathTransform translateX: 0 Y: 	1;
			scaleBy: 0.5 + ((frame *0.1) sin * 0.1 ).

		canvas pathTransform  rotateByDegrees: -180.
		1 to: 2 do:[ :i |
				canvas pathTransform  rotateByDegrees: 120 + ((frame * 0.1) cos * 20).
			canvas pathTransform  restoreAfter:  [
				self drawSpike: canvas.
			]
		].


		self drawCircles: canvas
	]
]

{ #category : #initialization }
AthensFlakeDemo >> initialize [

	super initialize.
	self extent: self defaultExtent.

	surface := AthensCairoSurface extent: self extent.

	circle := surface createPath: [:builder |
		builder
			absolute;
			moveTo: -1 @ 0 ;
			ccwArcTo: 0@ 1 angle: 90 degreesToRadians ;
			ccwArcTo: 1@0 angle: 90 degreesToRadians ;
			ccwArcTo: 0@ -1 angle: 90 degreesToRadians ;
			ccwArcTo: -1@0 angle:  90 degreesToRadians
	].

	spike := surface createPath: [:builder |

		builder
			absolute;
			moveTo:  -0.1 @ 0;
			lineTo: -0.05 @ 1;
			lineTo: 0.05 @ 1;
			lineTo: 0.1 @ 0
		].

	frame := 1.
	self startStepping
]

{ #category : #rendering }
AthensFlakeDemo >> render [

	surface drawDuring: [:canvas |
		canvas pathTransform loadIdentity.
		surface clear.

"		canvas
			setPaint: Color white;
			drawShape: (0@0 corner: self extent ).
"
		canvas pathTransform
			 translateX: self extent x *0.5  Y:  self extent y * 0.5;
			scaleBy: 160.

		canvas pathTransform  rotateByDegrees:  (frame * 0.5 + ((frame *0.1 + 1.7) cos * 5)).
		1 to: 3 do:[ :i |
			canvas pathTransform  rotateByDegrees: 120.
			self drawSpike: canvas.
		].
		 self drawCircles: canvas
	]
]

{ #category : #stepping }
AthensFlakeDemo >> step [
	frame := Time millisecondClockValue / 100.

	self changed
]

{ #category : #stepping }
AthensFlakeDemo >> stepTime [
	^ 0
]
