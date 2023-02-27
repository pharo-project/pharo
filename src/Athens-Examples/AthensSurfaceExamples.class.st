"
See examples on the class side and on class side of subclasses
"
Class {
	#name : #AthensSurfaceExamples,
	#superclass : #Model,
	#instVars : [
		'renderBlock'
	],
	#category : #'Athens-Examples-Base'
}

{ #category : #examples }
AthensSurfaceExamples class >> draw2Strings [
 	<example>
	| surf font1 font2 ascent advance |

	font1 := LogicalFont familyName: 'Arial' pointSize: 10.
	font2 := LogicalFont familyName: 'Tahoma' pointSize: 20.

	ascent := font1 getPreciseAscent max: font2 getPreciseAscent.

	surf := self newSurface: 600@200.

	surf drawDuring: [:can |
		surf clear: Color white.
		can pathTransform loadIdentity.

		can pathTransform translateX: 30 Y: 30;
		scaleBy: 1.9.

		can setPaint: (Color black).
		can setFont: font1.
		can pathTransform translateX: 0 Y: ascent.

		advance := can drawString: 'Keep '.
		can setFont: font2.

		"The #drawString: method answers a cumulative advance of rendered string,
		so by translating origin with advance, we can draw another string on 'same' line "
		can pathTransform translateBy: advance.

		advance := can drawString: 'IT'.
		can setFont: font1.
		can pathTransform translateBy: advance.
		can drawString: 'in mind.'].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> drawFontMetrics [
 	<example>
	| font |
	font := LogicalFont familyName: 'Arial' pointSize: 10.
	self openViewOn: [ :can |

		can pathTransform restoreAfter: [

		can pathTransform translateX: 30 Y: 30;
		scaleBy: 20.

		can setPaint: (Color r:1 g:0.9 b:0.9);
			drawShape: (0@0 corner: 400@ (font getPreciseHeight)).

		can setPaint: (Color r: 0.9 g: 0.9 b: 1);
			drawShape: (0@0 corner: 400@ (font getPreciseAscent+font getPreciseDescent)).

		(can setStrokePaint: Color green) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder
				moveTo: 0@0;
				lineTo: 0@font getPreciseAscent
			 ] ).

		(can setStrokePaint: Color red) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder
				moveTo: 0@font getPreciseAscent;
				lineTo: 0@font getPreciseDescent
			 ] ).

		(can setStrokePaint: Color blue) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder
				moveTo: 0@font getPreciseAscent;
				lineTo: 400@0
			 ]).

		can setFont: font.
		can setPaint: (Color black).
		can pathTransform translateX: 0 Y: (font getPreciseAscent).
		can drawString: 'yh'.
		]]
]

{ #category : #examples }
AthensSurfaceExamples class >> drawSetFillRule [
 	<example>
	self openViewOn: [ :canvas |
		| path |
		canvas pathTransform
			translateBy: 100 asPoint.
		"donut"
		canvas fillRuleEvenOdd.
		path := canvas createPath: [ :builder |
			builder
				absolute;
				moveTo: 50@0;
				arcCenterX: 0
					centerY: 0
					radius: 50
					startAngle: 0
					endAngle: Float pi * 2;
				moveTo: 30@0;
				arcCenterX: 0
					centerY: 0
					radius: 30
					startAngle: 0
					endAngle: Float pi * 2;
				close.
			 ].
		"CAIRO_FILL_RULE_WINDING := 0.
		CAIRO_FILL_RULE_EVEN_ODD := 1.
		self assert: canvas fillRule = 1"
		canvas setPaint: Color blue.
		canvas drawShape: path.
		canvas setStrokePaint: Color red.
		canvas drawShape: path.
		canvas pathTransform
			translateBy: 100 asPoint.
		canvas fillRuleWinding.
		canvas setPaint: Color red.
		canvas drawShape: path.
		canvas setStrokePaint: Color blue.
		canvas drawShape: path.
	]
]

{ #category : #examples }
AthensSurfaceExamples class >> example1 [

	self openViewOn: [ :can |
		can pathTransform restoreAfter: [

			can pathTransform translateX: 30 Y: 30.
			can pathTransform rotateByDegrees: 35.
			can setPaint: (Color red).

			can setShape: (-20@ -20 corner: 20@ 20).
			2 timesRepeat: [ can draw. can setPaint: (Color green alpha:0.5)].
			].
	]
]

{ #category : #examples }
AthensSurfaceExamples class >> example10 [
	"Draw a rounded rectangle, using the arc segments"

	| surf |
	surf := self newSurface: 440@440.

	surf drawDuring: [:can |
		| p |
		surf clear.
		can pathTransform
			scaleBy: 3;
			translateX: 10 Y: 10.
		p:= can createPath: [:path | |halfPi |
				halfPi := Float pi /2.
				path
					relative;
					moveTo: 10@0;
					lineTo: 40@0;
					ccwArcTo: 10@10 angle: halfPi;
					lineTo: 0@40;
					cwArcTo: -10@10 angle: halfPi;
					lineTo: -40@0;
					cwArcTo: -10@ -10 angle: halfPi;
					lineTo: 0@ -40;
					ccwArcTo: 10@ -10 angle: halfPi].

		can setShape: p.
		can setPaint: Color white.
		can draw.
		can setStrokePaint: Color red.
		can draw ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example2 [
	"Draw a path on surface."

	self openViewOn: [ :can |
		can setPaint: Color blue.

		can drawShape: (
			can createPath: [:path |
				path
					absolute;
					lineTo: 50@0;
					lineTo: 50@50;
					lineTo: 0@100
			])
	]
]

{ #category : #examples }
AthensSurfaceExamples class >> example3 [
	"Draw simple filled path, changing the transformation and colors to get some animated effects."

	| surf |
	surf := self newSurface: 800@800.

	surf drawDuring: [:can | | path |
		can setPaint: Color blue.

		can pathTransform translateX: 200 Y: 200.
		can pathTransform scaleBy: 6.

		path := can createPath: [:pathbuilder |
			pathbuilder
				absolute;
				moveTo: -25 @ -25;
				curveVia: 25@ -25 to: 25@25;
				curveVia: -25@25 to: -25@ -25
		].

		1 to: 1000 do: [:i |
			can setPaint: Color random.

			can pathTransform restoreAfter: [
				can pathTransform rotateByDegrees: i*5.
				can pathTransform scaleBy: (1 - (i/2000)).
				can drawShape: path ].
		]
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example3stroke [
	"Draw simple stroke  path, changing the transformation and colors to get some animated effects."

	| surf |
	surf := self newSurface: 400@400.

	surf drawDuring: [:can | |  path |
		(can setStrokePaint: Color blue) width: 1.

		can pathTransform translateX: 200 Y: 200.
		can pathTransform scaleBy: 5.

		path := can createPath: [:pathbuilder |
				pathbuilder
					absolute;
					moveTo: -25 @ -25;
					curveVia: 25@ -25 to: 25@25;
					curveVia: -25@25 to: -25@ -25 ].

		1 to: 1000 do: [:i |
			(can setStrokePaint: Color random) width: 1.
			can pathTransform restoreAfter: [
				can pathTransform rotateByDegrees: i*5.
				can pathTransform scaleBy: (1- ( i/2000)).
				can drawShape: path.
			]
		]
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example4 [
	"
	This example demostrates that same path could be reused multiple times when drawing.
	First, we creating a path, and later we can use it in #drawShape: command."

	| surf path |

	surf := self newSurface: 100@100.

	path :=  surf createRectanglePath: (0@ 0 corner: 20@ 20).

	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.
		can setPaint: (Color red).
		can pathTransform translateX: 5 Y: 5.
		can drawShape: path.
		can setPaint: (Color blue).
		can pathTransform translateX: 5 Y: 5.
		can drawShape: path.
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example5 [
	"Draw a hollow rectangle (frame) using lineTo/moveTo commands,
 	 with path, consisting of two contours: outer and inner one.

	 Note how #moveTo: command implicitly starts new contour
	 when inssued in the middle of command chain."

	| surf |
	surf := self newSurface: 100@100.

	surf drawDuring: [:can |
		surf clear.

		can setPaint: Color blue.

		can drawShape: (
			can createPath: [:path |
				path
					absolute;
					lineTo: 50@0;
					lineTo: 50@50;
					lineTo: 0@50;
					moveTo: 10@10;
					lineTo: 10@40;
					lineTo: 40@40;
					lineTo: 40@10
			])
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example6 [
	"Draw two rectangles, filled by linear gradient.

	 Note that it should produce same rectangles with exact same fills, because gradient paint
	 coordinates are affected by pathTransform matrix."

	| surf paint |
	surf := self newSurface: 100@200.

	paint := surf
		createLinearGradient: { 0->Color red. 1->Color green }
		start: 0@0
		stop: 50@50.

	surf drawDuring: [:can |
		surf clear.

		can setPaint: paint.

		can drawShape: (0@0 corner: 50@50).

		can pathTransform translateX: 50 Y: 50; rotateByDegrees: 30.
		can drawShape: (0@0 corner: 50@50) ].

		surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example6point1 [
	"Draw two rectangles, filled by linear gradient.

	Note that it should produce same rectangles with exact same fills, because gradient paint
	coordinates are affected by pathTransform matrix."

	| surf paint |
	surf := self newSurface: 100@200.

	paint := surf
		createLinearGradient: { 0->Color red. 1->Color green }
		start: 0@0
		stop: 30@30.

	surf drawDuring: [:can |
		surf clear.

		can setPaint: paint.

		can drawShape: (0@0 corner: 50@50) ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example6stroke [
	"Draw two rectangles, stoked by linear gradient.

	 Note that it should produce same rectangles with exact same fills, because gradient paint
	 coordinates are affected by pathTransform matrix."

	| surf paint |
	surf := self newSurface: 100@100.

	paint := surf
		createLinearGradient: { 0->Color red. 1->Color green }
		start: 0@0
		stop: 50@50.

	surf drawDuring: [:can | | stroke |
		surf clear.

		stroke := can setStrokePaint: paint.
		stroke width: 4.
		can drawShape: (0@0 corner: 50@50).

		can pathTransform translateX: 50 Y: 50.
		can drawShape: (0@0 corner: 50@50) ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example7 [
	"Draw two rectangles, filled by linear gradient.
	 Rotate the gradient in a loop.

	 Note that it should produce same rectangles with exact same fills, because gradient paint
	 coordinates are affected by pathTransform matrix."

	| surf paint |
	surf := self newSurface: 100@100.

	paint := surf
		createLinearGradient: { 0->Color red. 1->Color green }
		start: 0@0
		stop: 50@50.

	surf drawDuring: [:can |

		1 to: 10000 do: [:i |

			can pathTransform loadIdentity.

			can paintTransform loadIdentity translateX: 15 Y: 15; rotateByDegrees: (i/10000 * 360).
			can setPaint: paint.

			can drawShape: (0@0 corner: 50@50).

			can pathTransform translateX: 50 Y: 50.
			can drawShape: (0@0 corner: 50@50).

		].
	].
	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example8 [
	"Draw a rectangle filled by radial gradient."
	| surf paint |

	surf := self newSurface: 200@200.
	paint := surf
		createRadialGradient: { 0->Color red. 1->Color green }
		center: 100@100
		radius: 100.

	surf drawDuring: [:can |
			surf clear.

			can pathTransform loadIdentity.
			can setPaint: paint.

			can drawShape: (0@0 corner: 200@200)].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> example9 [
	"Fill the rectangle using image paint."

	| surf paint f  |
	surf := self newSurface: 200@200.
	surf asForm getCanvas fillRectangle: surf asForm boundingBox color: Color white.

	f := Form extent: 10@10 depth: 32.
	f getCanvas fillRectangle: (0@0 corner: 5@5) color: (Color red alpha: 0.1).
	f getCanvas fillRectangle: (5@5 corner: 10@10) color: (Color green alpha: 0.1).

	paint := surf createFormPaint: f.

	surf drawDuring: [:can |
		can
			setPaint: paint;
			drawShape: (0@0 corner: 100@200)].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleClip [

	| surf |
	surf := self newSurface: 100@100.

	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.
		can setPaint: (Color blue).
		can drawShape: (0@0 corner: 100@ 100).

		can pathTransform translateX: -20 Y: -20.
		can clipBy: (20@20 corner: 80@80) during: [
			can pathTransform translateX: 20 Y: 20.
			can setPaint: (Color red).
			can drawShape: (0@0 corner: 100@ 100).
		]
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleConicalGradient [
	| surface meshGradient center radius sliceCount sliceSize colorAngleOffset |
	surface := AthensCairoSurface extent: 512@512.
	colorAngleOffset := -90.0.
	sliceCount := 16.
	sliceSize := Float twoPi / sliceCount.
	center := 256@256.
	radius := 256.

	meshGradient := MeshGradientPaint new.
	0 to: sliceCount - 1 do: [ :sliceIndex |
		| sliceStartAngle sliceEndAngle sliceStartCenterColor sliceStartRadiusColor sliceEndCenterColor sliceEndRadiusColor |
		sliceStartAngle := sliceIndex * sliceSize.
		sliceEndAngle := (sliceIndex + 1) * sliceSize.

		sliceStartCenterColor := Color h: sliceStartAngle radiansToDegrees + colorAngleOffset s: 0.0 v: 1.0.
		sliceStartRadiusColor := Color h: sliceStartAngle radiansToDegrees + colorAngleOffset s: 1.0 v: 1.0.
		sliceEndCenterColor := Color h: sliceEndAngle radiansToDegrees + colorAngleOffset s: 0.0 v: 1.0.
		sliceEndRadiusColor := Color h: sliceEndAngle radiansToDegrees + colorAngleOffset s: 1.0 v: 1.0.

		meshGradient addCircleSliceCenter: center radius: radius startAngle: sliceStartAngle endAngle: sliceEndAngle colors: {
			sliceStartCenterColor . sliceStartRadiusColor.
			sliceEndRadiusColor . sliceEndCenterColor.
		}
	].

	surface drawDuring: [ :canvas |
		canvas surface clear: Color white.

		canvas
			setPaint: meshGradient;
			setShape: (0@0 extent: 512@512);
			draw
	].

	surface asForm asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleCoonsPatch [
	| surface meshPaint |
	surface := AthensCairoSurface extent: 300@300.
	meshPaint := MeshGradientPaint new
		addCoonsPatchWithPoints: {
			0@0.
			30@ -30.  60@30. 100@0.
			60@  30. 130@60. 100@100.
			60@  70.  30@130.   0@100.
			30@  70. -30@30
		} colors: { Color red . Color green . Color blue . Color yellow}.

	surface drawDuring: [ :canvas |
		canvas surface clear: Color white.
		canvas pathTransform translateBy: 100@100.

		canvas
			setPaint: meshPaint;
			setShape: (-100 @ -100 extent: 300@300);
			draw
	].

	surface asForm asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleCoonsPatch2 [
	| surface meshPaint |
	surface := AthensCairoSurface extent: 300@300.
	meshPaint := MeshGradientPaint new
		buildPatchWith: [ :patchBuilder |
			patchBuilder
				moveTo: 0@0;
				curveVia: 30@ -30 and: 60@30 to: 100@0;
				curveVia: 60@  30 and: 130@60 to: 100@100;
				curveVia: 60@  70 and: 30@130 to: 0@100;
				curveVia: 30@  70 and: -30@30 to: 0@0;
				colors: { Color red . Color green . Color blue . Color yellow}
		].

	surface drawDuring: [ :canvas |
		canvas surface clear: Color white.
		canvas pathTransform translateBy: 100@100.

		canvas
			setPaint: meshPaint;
			setShape: (-100 @ -100 extent: 300@300);
			draw
	].

	surface asForm asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleDrawForm [

	| surf surf2   |
	surf2 := self newSurface: 100@100.
	surf2 drawDuring: [:c |
		c setPaint: (Color red alpha: 0.5) .
		c drawShape: (0@0 corner: 100@100).
		].

	surf := self newSurface: 300@300.

	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.

		can setShape: (0@0 corner: 300@300).
		can setPaint: Color black.
		can draw.
		can pathTransform translateX: 30 Y: 30.
		can setPaint: surf2.
		can draw.

		can pathTransform translateX: 30 Y: 30.
		can draw ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleDrawForm2 [

	| surf  |
	surf := self newSurface: 300@300.

	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.

		can setShape: (0@0 corner: 300@300).
		can setPaint: Color black.
		can draw.
		can pathTransform scaleBy: 0.5;
		 translateX: 60 Y: 60.
		(can setPaint: (self iconNamed: #helpIcon)) repeat.
		can paintTransform scaleBy: 3; rotateByDegrees: 30.
		can draw ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleDrawText [

	| surf  font |
	font := LogicalFont familyName: 'Arial' pointSize: 20.

	surf := self newSurface: 300@100.

	surf drawDuring: [:can |
		"clear background"
		surf clear: Color white.

		"set font and color"
		can setFont: font.
		can setPaint: Color black.

		"translate an origin by font's ascent, otherwise
		we will see only things below baseline"
		can pathTransform translateX: 0 Y: (font getPreciseAscent).
		can drawString: 'Hello Athens!' ].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleGouraudTriangle [
	| surface meshPaint |
	surface := AthensCairoSurface extent: 512@512.
	meshPaint := MeshGradientPaint new
		addTriangleWithPoints: { 256@50 . 450@450 . 50@450 }
			colors: { Color red . Color green . Color blue}.

	surface drawDuring: [ :canvas |
		canvas surface clear: Color white.

		canvas
			setPaint: meshPaint;
			setShape: (50@50 extent: 400@400);
			draw
	].

	surface asForm asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleGouraudTriangle2 [
	| surface meshPaint |
	surface := AthensCairoSurface extent: 512@512.
	meshPaint := MeshGradientPaint new
		buildPatchWith: [ :patchBuilder |
			patchBuilder
				moveTo: 256@50;
				lineTo: 450@450;
				lineTo: 50@450;
				colors: { Color red . Color green . Color blue}
		].

	surface drawDuring: [ :canvas |
		canvas surface clear: Color white.

		canvas
			setPaint: meshPaint;
			setShape: (50@50 extent: 400@400);
			draw
	].

	surface asForm asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleNestedClip [

	| surf |
	surf := self newSurface: 100@100.

	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.
		can setPaint: (Color blue).
		can drawShape: (0@0 corner: 100@ 100).

		can pathTransform translateX: -20 Y: -20.
		can clipBy: (20@20 corner: 80@80) during: [
			can pathTransform
				translateX: 20 Y: 20;
				rotateByDegrees: -20.
			can setPaint: (Color red).
			can drawShape: (0@0 corner: 100@ 100).

			can clipBy: (20@20 corner: 80@80) during: [
				can pathTransform translateX: 20 Y: 20.
				can setPaint: (Color green).
				can drawShape: (0@0 corner: 100@ 100).
			]
		]
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleStrokeRect [
	"Draw a frame rectangle, rotate & transform it in a loop"

	| surf |
	surf := self newSurface: 500@500.
	surf drawDuring: [:can |
		can pathTransform translateX: 200 Y: 200.
		can pathTransform scaleBy: 8.

		1 to: 1000 do: [:i |
			(can setStrokePaint: Color random) width: 1.
			can pathTransform restoreAfter: [
				can pathTransform rotateByDegrees: i*5.
				can pathTransform scaleBy: (1- ( i/2000)).
				can drawShape: (0@0 corner: 25@25). ].
		]
	].

	surf asMorph openInWindow
]

{ #category : #examples }
AthensSurfaceExamples class >> exampleUseForm [

	| surf  form |
	form := Form extent: 100@100 depth: 32.
	form getCanvas fillRectangle: (0@0 corner: 30@30)  color: (Color red).
	form getCanvas fillRectangle: (10@10 corner: 40@40)  color: (Color green).
	form getCanvas fillRectangle: (20@20 corner: 50@50)  color: (Color blue).

	surf := AthensCairoSurface fromForm: form.
	surf asMorph openInWindow
]

{ #category : #'instance creation' }
AthensSurfaceExamples class >> newAsSceneFrom: aBlock [
	^ self new
		renderBlock: aBlock;
		yourself
]

{ #category : #utilities }
AthensSurfaceExamples class >> newSurface:  extent [
	^ AthensCairoSurfaceExamples newSurface: extent
]

{ #category : #'opening view' }
AthensSurfaceExamples class >> openViewOn: aBlock [

	^ AthensSceneView new
		scene: (self newAsSceneFrom:aBlock);
		openInWindow
]

{ #category : #'surface management' }
AthensSurfaceExamples >> createSurface: anExtent [
	^ self class newSurface: anExtent
]

{ #category : #accessing }
AthensSurfaceExamples >> renderBlock [

	^ renderBlock
]

{ #category : #accessing }
AthensSurfaceExamples >> renderBlock: anObject [

	renderBlock := anObject
]

{ #category : #rendering }
AthensSurfaceExamples >> renderOn: aCanvas [
	renderBlock value: aCanvas
]
