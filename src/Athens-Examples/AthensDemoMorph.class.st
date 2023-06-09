"
Demos for Athens, just run:

	AthensDemoPresenter open

You can find an implementation of each figure in
corresponding #figure<#>: method
"
Class {
	#name : #AthensDemoMorph,
	#superclass : #Morph,
	#instVars : [
		'surface',
		'current',
		'frame',
		'pharoLogo',
		'esugBallon'
	],
	#category : #'Athens-Examples-Morphic'
}

{ #category : #utilities }
AthensDemoMorph class >> roundedRectanglePathOn: aCanvas [

	^ aCanvas createPath: [:builder |
		builder
			absolute;
			moveTo: 70@100;
			lineTo: 330@100;
			cwArcTo: 350@120 angle: 90 degreesToRadians;
			lineTo: 350@280;
			cwArcTo: 330@300 angle: 90 degreesToRadians;
			lineTo: 70@300;
			cwArcTo: 50@280 angle: 90 degreesToRadians;
			lineTo: 50@120;
			cwArcTo: 70@100 angle: 90 degreesToRadians ]
]

{ #category : #accessing }
AthensDemoMorph >> backColor [
	^ Color white
]

{ #category : #accessing }
AthensDemoMorph >> carColor [
	^ Color veryVeryLightGray
]

{ #category : #accessing }
AthensDemoMorph >> chasisDecoratorColor [
	^ Color black
]

{ #category : #accessing }
AthensDemoMorph >> chasisLineColor [
	^ Color white
]

{ #category : #accessing }
AthensDemoMorph >> coloredCarColor [
	^ Color red
]

{ #category : #accessing }
AthensDemoMorph >> defaultExtent [
	^ 400@400
]

{ #category : #drawing }
AthensDemoMorph >> drawBackgroundOn: aCanvas [
	aCanvas
		setPaint: Color veryLightGray;
		drawShape: (0 @ 0 extent: 1 @ 1)
]

{ #category : #drawing }
AthensDemoMorph >> drawCarOn: aCanvas [

	| p stroke wheel decorator |
	"chasis"
	p := aCanvas createPath: [:b |
			b
				moveTo: 0.3@0.3 ;
				lineTo: 0.4@0;
				curveVia: 0.1@0 to: 0.1@0.1;
				lineTo: 0@0.2;
				curveVia: 0@0.1 to: -0.1@0.1;
				lineTo: -0.4@0;
				curveVia: -0.1@0 to: (0.1@0.1) negated;
				lineTo: 0@0.2 negated ;
				curveVia: 0@0.1 negated to: 0.1@0.1 negated ].
		aCanvas
			setPaint: self coloredCarColor;
			drawShape: p.
		stroke := aCanvas setStrokePaint: self wheelColor.
		stroke width: 0.01.
		aCanvas draw.

		"wheels"
		wheel := aCanvas createPath:[:b|
			b
				moveTo: 	0.3@0.3;
				cwArcTo: 0.15@0 angle: Float halfPi ;
				moveTo: 	0.1@0;
				cwArcTo: 0.15@0 angle: Float halfPi ;
				moveTo: 	0.0@0.4;
				cwArcTo: 0.15 negated@0 angle: Float halfPi;
				moveTo: 	-0.1@0;
				cwArcTo: 0.15 negated@0 angle: Float halfPi 	].
		aCanvas
			setPaint: self wheelColor;
			drawShape: wheel.

		"chasis decorator"
		decorator  := aCanvas createPath:[:b|
			b
				moveTo: 	0.2@0.45;
				lineTo: 0.225@0 ;
				cwArcTo: 0.15@0 angle: Float halfPi ;
				lineTo: 	0.225@0;
				lineTo: 0@0.1 ;
				lineTo: -0.225@ 0;
				cwArcTo: -0.15@0 angle: Float halfPi ;
				lineTo: -0.225@ 0	;
				lineTo: 0@0.1 negated].
		stroke := aCanvas setStrokePaint: self chasisDecoratorColor.
		stroke width: 0.01.
		aCanvas drawShape: decorator.
		aCanvas setPaint: self chasisLineColor.
		aCanvas draw
]

{ #category : #drawing }
AthensDemoMorph >> drawOn: aCanvas [

	self render.

	aCanvas
		drawImage: surface asForm at: self bounds origin
]

{ #category : #figures }
AthensDemoMorph >> figure10: aCanvas [
	| ellipsePath patternPaint rectangle |
	ellipsePath := aCanvas createPath: [ :builder |
		builder
			moveTo: 0@200;
			cwArcTo:  240@0 angle: Float pi;
			cwArcTo: -240@0 angle: Float pi ].
	aCanvas setPaint: Color black.
	aCanvas drawShape: ellipsePath.
	rectangle := 250@40 extent: 140@300.
	aCanvas drawShape: rectangle.

	aCanvas paintTransform loadIdentity.
	aCanvas paintTransform scaleBy: ((frame/100) sin abs) * 5.
	aCanvas paintTransform rotateByDegrees: (frame/100) sin * 360.

	patternPaint := aCanvas setPaint: pharoLogo.
	patternPaint setExtend: #Repeat.
	aCanvas drawShape: ellipsePath.

	aCanvas setPaint: patternPaint.
	patternPaint setExtend: #Reflect.
	aCanvas drawShape: rectangle
]

{ #category : #figures }
AthensDemoMorph >> figure1: aCanvas [
	aCanvas
		setPaint: self carColor;
		drawShape: (50@100 extent: 300@200)
]

{ #category : #figures }
AthensDemoMorph >> figure2: aCanvas [
	| path |
	path := aCanvas createPath: [:builder |
		builder
			absolute;
			moveTo: 70@100;
			lineTo: 330@100;
			cwArcTo: 350@120 angle: 90 degreesToRadians;
			lineTo: 350@280;
			cwArcTo: 330@300 angle: 90 degreesToRadians;
			lineTo: 70@300;
			cwArcTo: 50@280 angle: 90 degreesToRadians;
			lineTo: 50@120;
			cwArcTo: 70@100 angle: 90 degreesToRadians ].

	aCanvas
		setPaint: self carColor;
		drawShape: path
]

{ #category : #figures }
AthensDemoMorph >> figure3: aCanvas [
	| path |
	path := aCanvas createPath: [:builder |
		builder
			relative;
			moveTo: 0.3@0.3 ;
			lineTo: 0.4@0;
			curveVia: 0.1@0 to: 0.1@0.1;
			lineTo: 0@0.2;
			curveVia: 0@0.1 to: -0.1@0.1;
			lineTo: -0.4@0;
			curveVia: -0.1@0 to: (-0.1@ -0.1);
			lineTo: 0@ -0.2  ;
			curveVia: 0@ -0.1 to: 0.1@ -0.1 ].

	aCanvas pathTransform scaleBy: self extent.
	aCanvas
		setPaint: self carColor;
		drawShape: path
]

{ #category : #figures }
AthensDemoMorph >> figure4: aCanvas [
	| path stroke |

	path := self class roundedRectanglePathOn: aCanvas.

	aCanvas setShape: path.

	aCanvas setPaint: self carColor.
	aCanvas draw.

	stroke := aCanvas setStrokePaint: Color black.
	stroke width: 3.

	aCanvas draw
]

{ #category : #figures }
AthensDemoMorph >> figure5: aCanvas [
	| wheels |
	self figure4: aCanvas.

	wheels := aCanvas createPath: [:builder |
		builder
			moveTo:  100@100;
			cwArcTo: 60@0 angle: 90 degreesToRadians;
			moveTo:  80@0;
			cwArcTo: 60@0 angle: 90 degreesToRadians;
			moveTo:  0.0@200;
			cwArcTo: -60@0 angle: 90 degreesToRadians;
			moveTo:  -80@0;
			cwArcTo: -60@0 angle: 90 degreesToRadians ].

	aCanvas
		setPaint: self wheelColor;
		drawShape: wheels
]

{ #category : #figures }
AthensDemoMorph >> figure6: aCanvas [
	| linearGradient |

	self figure5: aCanvas.

	linearGradient :=
		aCanvas surface createLinearGradient: {
			0 -> self coloredCarColor .
			0.25 -> self coloredCarColor muchLighter.
			0.5 -> Color white.
			0.75 -> (self coloredCarColor muchLighter ).
			1 -> (self coloredCarColor alpha: 0)
		}
		start:  0@100
		stop: 0@300.

	aCanvas setShape: (self class roundedRectanglePathOn: aCanvas).

	aCanvas setPaint: linearGradient.

	aCanvas draw
]

{ #category : #figures }
AthensDemoMorph >> figure7: aCanvas [
	| pt |

	self drawBackgroundOn: aCanvas.

	pt := aCanvas pathTransform.

	pt restoreAfter:[
		pt scaleBy: self extent .
		self drawCarOn: aCanvas.

		pt scaleBy: 0.25@0.25.
		self drawCarOn: aCanvas.

		pt  translateBy: 2@0; rotateByDegrees: 35.
		self drawCarOn: aCanvas ]
]

{ #category : #figures }
AthensDemoMorph >> figure8: aCanvas [
	| pt |
	pt := aCanvas pathTransform.
	pt restoreAfter:[
		pt scaleBy: self extent.
		self drawBackgroundOn: aCanvas.

		pt scaleBy: (frame/100) sin abs.
		self drawCarOn: aCanvas.
		pt scaleBy: 0.25@0.2.5.
		self drawCarOn: aCanvas.

		pt  translateBy: 2@0; rotateByDegrees: 35.
		self drawCarOn: aCanvas ]
]

{ #category : #figures }
AthensDemoMorph >> figure9: aCanvas [
	| ellipsePath patternPaint |
	ellipsePath := aCanvas createPath: [ :builder |
		builder
			moveTo: 10@200;
			cwArcTo: 360@0  angle: Float pi ;
			lineTo: -100@180;
			lineTo: -150@ -0;
			lineTo: -110@ -180].

	aCanvas setPaint: Color black.
	aCanvas drawShape: ellipsePath.

	aCanvas paintTransform loadIdentity.
	aCanvas paintTransform scaleBy: ((frame/200) sin abs)*3.


	patternPaint := aCanvas setPaint: esugBallon .
	patternPaint setExtend: #None.
	aCanvas draw
]

{ #category : #accessing }
AthensDemoMorph >> figures [
	^ #(
	figure1:
	figure2:
	figure3:
	figure4:
	figure5:
	figure6:
	figure7:
	figure8:
	figure9:
	figure10:
	)
]

{ #category : #initialization }
AthensDemoMorph >> initialize [
	| ref loadFileFailBlock |
	super initialize.
	self extent: self defaultExtent.
	self color: self backColor.
	surface := AthensCairoSurface extent: self extent.
	current := 1.
	frame := 0.
	loadFileFailBlock := AthensCairoSurface fromForm: (Form fromDisplay: (0 @ 0 extent: 300 @ 150)).
	ref := 'Pharo.png' asFileReference.
	pharoLogo := AthensCairoSurface createFromFile: ref fullName ifFailed: [ loadFileFailBlock ].

	ref := 'esug-balloon.png' asFileReference.
	esugBallon := AthensCairoSurface createFromFile: ref fullName ifFailed: [ loadFileFailBlock ].

	self startStepping
]

{ #category : #accessing }
AthensDemoMorph >> nextFigure [

	current := current + 1.

	current > self figures size ifTrue: [ current := 1 ]
]

{ #category : #accessing }
AthensDemoMorph >> prevFigure [

	current := current - 1.

	current < 1 ifTrue: [ current := self figures size ]
]

{ #category : #drawing }
AthensDemoMorph >> render [

	surface drawDuring: [:canvas |
		surface clear.
		canvas pathTransform loadIdentity.
		canvas setPaint: Color lightGray;
		drawShape: (0@0 corner: self extent).
		self perform: (self figures at: current)  with: canvas
	]
]

{ #category : #stepping }
AthensDemoMorph >> step [
	frame := frame + 1.
	self changed
]

{ #category : #stepping }
AthensDemoMorph >> stepTime [
	^ 0
]

{ #category : #drawing }
AthensDemoMorph >> wheelColor [
	^ Color brown darker
]
