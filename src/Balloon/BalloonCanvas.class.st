"
BalloonCanvas is a canvas using the BalloonEngine for drawing wherever possible. It has various methods which other canvases do not support due to the extra features of the balloon engine.
"
Class {
	#name : #BalloonCanvas,
	#superclass : #FormCanvas,
	#instVars : [
		'transform',
		'colorTransform',
		'engine',
		'aaLevel',
		'deferred'
	],
	#category : #'Balloon-Canvas'
}

{ #category : #accessing }
BalloonCanvas >> aaLevel [
	^aaLevel
]

{ #category : #accessing }
BalloonCanvas >> aaLevel: newLevel [
	"Only allow changes to aaLevel if we're working on >= 8 bit forms"
	form depth >= 8 ifFalse:[^self].
	aaLevel = newLevel ifTrue:[^self].
	self flush.	"In case there are pending primitives in the engine"
	aaLevel := newLevel.
	engine ifNotNil:[engine aaLevel: aaLevel].
]

{ #category : #converting }
BalloonCanvas >> asBalloonCanvas [
	^self
]

{ #category : #transforming }
BalloonCanvas >> colorTransformBy: aColorTransform [
	aColorTransform ifNil:[^self].
	colorTransform 
		ifNil:[colorTransform := aColorTransform]
		ifNotNil:[colorTransform := colorTransform composedWithLocal: aColorTransform]
]

{ #category : #accessing }
BalloonCanvas >> deferred [
	^deferred
]

{ #category : #accessing }
BalloonCanvas >> deferred: aBoolean [
	deferred == aBoolean ifTrue:[^self].
	self flush. "Force pending prims on screen"
	deferred := aBoolean.
	engine ifNotNil:[engine deferred: aBoolean].
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawBezier3Shape: vertices color: c borderWidth: borderWidth borderColor:
borderColor [
	self drawBezierShape: (Bezier3Segment convertBezier3ToBezier2:
vertices) color: c borderWidth: borderWidth borderColor: borderColor
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawBezierShape: vertices color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a boundary shape that is defined by a list of vertices.
	Each three subsequent vertices define a quadratic bezier segment.
	For lines, the control point should be set to either the start or the end
	of the bezier curve."

	self ensuredEngine
		drawBezierShape: vertices
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawCompressedShape: compressedShape [
	"Draw a compressed shape"
	self ensuredEngine
		drawCompressedShape: compressedShape
		transform: transform.
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawGeneralBezier3Shape: contours color: c borderWidth: borderWidth
borderColor: borderColor [
	| b2 |
	b2 := contours collect: [:b3 | Bezier3Segment
convertBezier3ToBezier2: b3 ].
	self drawGeneralBezierShape: b2 color: c borderWidth: borderWidth
borderColor: borderColor
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawGeneralBezierShape: contours color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a general boundary shape (e.g., possibly containing holes)"

	self ensuredEngine
		drawGeneralBezierShape: contours
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawGeneralPolygon: contours color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a general polygon (e.g., a polygon that can contain holes)"

	self ensuredEngine
		drawGeneralPolygon: contours
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawOval: r color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw the oval defined by the given rectangle"

	self ensuredEngine
		drawOval: r
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #'drawing-polygons' }
BalloonCanvas >> drawPolygon: vertices fillStyle: aFillStyle [
	"Fill the given polygon."
	self drawPolygon: vertices fillStyle: aFillStyle borderWidth: 0 borderColor: nil
]

{ #category : #'drawing-polygons' }
BalloonCanvas >> drawPolygon: vertices fillStyle: aFillStyle borderWidth: borderWidth borderColor: borderColor [
	"Draw a simple polygon defined by the list of vertices."

	self ensuredEngine
		drawPolygon: (vertices copyWith: vertices first)
		fill: aFillStyle
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #'balloon drawing' }
BalloonCanvas >> drawRectangle: r color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a rectangle"

	self ensuredEngine
		drawRectangle: r
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.
]

{ #category : #todo }
BalloonCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [
	(self ifNoTransformWithIn: boundsRect)
		ifTrue:[^super drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c]
]

{ #category : #todo }
BalloonCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	(self ifNoTransformWithIn: boundsRect)
		ifTrue:[^super drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc]
]

{ #category : #accessing }
BalloonCanvas >> ensuredEngine [
	engine ifNil:[
		engine := BalloonEngine new.
		"engine := BalloonDebugEngine new"
		engine aaLevel: aaLevel.
		engine bitBlt: port.
		engine destOffset: origin.
		engine clipRect: clipRect.
		engine deferred: deferred.
		engine].
	engine colorTransform: colorTransform.
	engine edgeTransform: transform.
	^engine
]

{ #category : #drawing }
BalloonCanvas >> fillColor: c [
	"Note: This always fills, even if the color is transparent."
	"Note2: To achieve the above we must make sure that c is NOT transparent"
	self frameAndFillRectangle: form boundingBox 
		fillColor: (c alpha: 1.0)
		borderWidth: 0
		borderColor: nil
]

{ #category : #drawing }
BalloonCanvas >> fillOval: r color: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a filled and outlined oval"
	"Note: The optimization test below should actually read:
		self ifNoTransformWithIn: (r insetBy: borderWidth // 2)
	but since borderWidth is assumed to be very small related to r we don't check it."

	(self ifNoTransformWithIn: r)
		ifTrue:[^super fillOval: r color: c borderWidth: borderWidth borderColor: borderColor].

	^self drawOval: (r insetBy: borderWidth // 2) 
			color: c 
			borderWidth: borderWidth 
			borderColor: borderColor
]

{ #category : #'drawing-ovals' }
BalloonCanvas >> fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Fill the given rectangle."
	^self drawOval: (aRectangle insetBy: bw // 2)
			color: aFillStyle 
			borderWidth: bw
			borderColor: bc

]

{ #category : #'balloon drawing' }
BalloonCanvas >> fillRectangle: aRectangle basicFillStyle: aFillStyle [
	"Fill the given rectangle with the given, non-composite, fill style."
	
	^self drawRectangle: aRectangle
			color: aFillStyle
			borderWidth: 0
			borderColor: nil

]

{ #category : #drawing }
BalloonCanvas >> fillRectangle: r color: c [
	"Fill the rectangle with the given color"
	^self frameAndFillRectangle: r
			fillColor: c
			borderWidth: 0
			borderColor: nil
]

{ #category : #initialize }
BalloonCanvas >> flush [
	"Force all pending primitives onscreen"
	engine ifNotNil:[engine flush].
]

{ #category : #drawing }
BalloonCanvas >> frameAndFillRectangle: r fillColor: c borderWidth: borderWidth borderColor: borderColor [
	"Draw a filled and outlined rectangle"
	"Note: The optimization test below should actually read:
		self ifNoTransformWithIn: (r insetBy: borderWidth // 2)
	but since borderWidth is assumed to be very small related to r we don't check it."

	(self ifNoTransformWithIn: r)
		ifTrue:[^super frameAndFillRectangle: r fillColor: c borderWidth: borderWidth borderColor: borderColor].

	^self drawRectangle: (r insetBy: borderWidth // 2) 
			color: c 
			borderWidth: borderWidth 
			borderColor: borderColor
]

{ #category : #drawing }
BalloonCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor [
	"Draw a beveled or raised rectangle"
	| bw |

	"Note: The optimization test below should actually read:
		self ifNoTransformWithIn: (r insetBy: borderWidth // 2)
	but since borderWidth is assumed to be very small related to r we don't check it."

	(self ifNoTransformWithIn: r)
		ifTrue:[^super frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor].

	"Fill rectangle and draw top and left border"
	bw := borderWidth // 2.
	self drawRectangle: (r insetBy: bw)
		color: fillColor
		borderWidth: borderWidth
		borderColor: topLeftColor.
	"Now draw bottom right border."
	self drawPolygon: (Array with: r topRight + (bw negated@bw) 
							with: r bottomRight - bw asPoint
							with: r bottomLeft + (bw@bw negated))
		color: nil
		borderWidth: borderWidth
		borderColor: bottomRightColor.
]

{ #category : #private }
BalloonCanvas >> ifNoTransformWithIn: box [
	"Return true if the current transformation does not affect the given bounding box"
	| delta |
	transform ifNil: [^true].
	delta := (transform localPointToGlobal: box origin) - box origin.
	^(transform localPointToGlobal: box corner) - box corner = delta
]

{ #category : #private }
BalloonCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	| warp dstRect srcQuad dstOffset |
	(self ifNoTransformWithIn: sourceRect) & false
		ifTrue:[^super image: aForm at: aPoint sourceRect: sourceRect rule: rule].
	dstRect := (transform localBoundsToGlobal: (aForm boundingBox translateBy: aPoint)).
	dstOffset := 0@0. "dstRect origin."
	"dstRect := 0@0 corner: dstRect extent."
	srcQuad := transform globalPointsToLocal: (dstRect innerCorners).
	srcQuad := srcQuad collect:[:pt| pt - aPoint].
	warp := (WarpBlt toForm: form)
			sourceForm: aForm;
			cellSize: 2;  "installs a new colormap if cellSize > 1"
			combinationRule: Form over.
	warp copyQuad: srcQuad toRect: (dstRect translateBy: dstOffset).

	self frameRectangle: (aForm boundingBox translateBy: aPoint) color: Color green.

	"... TODO ... create a bitmap fill style from the form and use it for a simple rectangle."
]

{ #category : #initialization }
BalloonCanvas >> initialize [
	super initialize.
	aaLevel := 1.
	deferred := false.
]

{ #category : #testing }
BalloonCanvas >> isBalloonCanvas [
	^true
]

{ #category : #testing }
BalloonCanvas >> isVisible: aRectangle [
	^transform 
		ifNil:[super isVisible: aRectangle]
		ifNotNil:[super isVisible: (transform localBoundsToGlobal: aRectangle)]
]

{ #category : #drawing }
BalloonCanvas >> line: pt1 to: pt2 width: w color: c [
	"Draw a line from pt1 to: pt2"

	(aaLevel = 1 and: [self ifNoTransformWithIn:(pt1 rectangle: pt2)])
		ifTrue:[^super line: pt1 to: pt2 width: w color: c].
	^self drawPolygon: (Array with: pt1 with: pt2)
		color: c
		borderWidth: w
		borderColor: c
]

{ #category : #todo }
BalloonCanvas >> paragraph: para bounds: bounds color: c [
	(self ifNoTransformWithIn: bounds)
		ifTrue:[^super paragraph: para bounds: bounds color: c].
]

{ #category : #copying }
BalloonCanvas >> postCopy [
	self flush.
	super postCopy.
	self resetEngine
]

{ #category : #transforming }
BalloonCanvas >> preserveStateDuring: aBlock [
	| state result |
	state := BalloonState new.
	state transform: transform.
	state colorTransform: colorTransform.
	state aaLevel: self aaLevel.
	result := aBlock value: self.
	transform := state transform.
	colorTransform := state colorTransform.
	self aaLevel: state aaLevel.
	^result
]

{ #category : #initialize }
BalloonCanvas >> resetEngine [
	engine := nil.
]

{ #category : #transforming }
BalloonCanvas >> transformBy: aTransform [
	aTransform ifNil:[^self].
	transform 
		ifNil:[transform := aTransform]
		ifNotNil:[transform := transform composedWithLocal: aTransform]
]

{ #category : #transforming }
BalloonCanvas >> transformBy: aDisplayTransform during: aBlock [
	| myTransform result |
	myTransform := transform.
	self transformBy: aDisplayTransform.
	result := aBlock value: self.
	transform := myTransform.
	^result
]
