"
I am a wrapper for a (Form-)Canvas that implements the Canvas API on top of an AthensCanvas.
"
Class {
	#name : #AthensCanvasWrapper,
	#superclass : #Object,
	#instVars : [
		'athensCanvas',
		'currentClipRect',
		'origin'
	],
	#category : #'Athens-Morphic'
}

{ #category : #'instance creation' }
AthensCanvasWrapper class >> on: anAthensCanvas [
	^ self new
		canvas: anAthensCanvas;
		yourself
]

{ #category : #accessing }
AthensCanvasWrapper >> allocateForm: extentPoint [
	^ Form extent: extentPoint depth:32
]

{ #category : #'canvas converting' }
AthensCanvasWrapper >> asShadowDrawingCanvas [
	"this may not work, but it
	doesn't hurt yet and HandMorph relies on this"

	^ (ShadowDrawingCanvas on: self) shadowColor: (Color orange alpha: 0.2)
]

{ #category : #private }
AthensCanvasWrapper >> canvas [
	^ athensCanvas
]

{ #category : #accessing }
AthensCanvasWrapper >> canvas: anAthensCanvas [
	athensCanvas := anAthensCanvas
]

{ #category : #'private - clip transform' }
AthensCanvasWrapper >> canvasClipBy: aClipRect during: aBlock [
	| oldRect |
	oldRect := self clipRect.
	currentClipRect := aClipRect.
	[ self canvas clipBy: currentClipRect during:[ aBlock value ]]
		ensure: [ currentClipRect := oldRect ]
]

{ #category : #'canvas drawing - support' }
AthensCanvasWrapper >> clipBy: aRectangle during: aBlock [
	self canvasClipBy: aRectangle during: [ aBlock value: self ]
]

{ #category : #'canvas accessing' }
AthensCanvasWrapper >> clipRect [
	^ self canvas clipRect
]

{ #category : #'private - clip transform' }
AthensCanvasWrapper >> clipWrapperAndAthensTransformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [
"the clipTransform methods are just trial and error attemps on getting this right"
	| rect |
	rect := aDisplayTransform globalBoundsToLocal: aClipRect.
	self
		clipBy: rect
		during: [ :c |
			c canvas pathTransform
				restoreAfter: [
					"c canvas morphicClipBy: rect during: [ "
					c canvas pathTransform translateBy: aDisplayTransform offset negated.
					c canvas pathTransform rotateByRadians: aDisplayTransform angle negated.
					c  canvas pathTransform scaleBy: aDisplayTransform scale.
					aBlock value: self ] ]	"]"
]

{ #category : #'private - clip transform' }
AthensCanvasWrapper >> clipWrapperTransformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [
"the clipTransform methods are just trial and error attemps on getting this right"
	| rect |
	rect := aDisplayTransform globalBoundsToLocal: aClipRect.
	self canvas pathTransform
		restoreAfter: [
			self canvas pathTransform translateBy: aDisplayTransform offset negated.
			self canvas pathTransform rotateByRadians: aDisplayTransform angle negated.
			self canvas pathTransform scaleBy: aDisplayTransform scale.
			self clipBy: rect during: [ :c | aBlock value: c ]]
]

{ #category : #'canvas accessing' }
AthensCanvasWrapper >> contentsOfArea: aRectangle into: aForm [
	"this may conflict with current pathtransformations, but up to now"
	"it works"
	| bb |
	self flush.
	bb := BitBlt toForm: aForm.
	bb
		sourceForm: self form;
		combinationRule: Form over;
		sourceX: aRectangle left + origin x;
		sourceY: aRectangle top + origin y;
		width: aRectangle width;
		height: aRectangle height;
		copyBits.
	^ aForm
]

{ #category : #'formcanvas copying' }
AthensCanvasWrapper >> copyClipRect: aRectangle [
"dont know why this exists, all calls with a different cliprect should go through"
"clipBy:during: and I don't know if this properly works with athens canvases because"
"we can not handle to different clipstates for the same canvas."
	^ self copyOrigin: origin clipRect: aRectangle
]

{ #category : #'formcanvas copying' }
AthensCanvasWrapper >> copyOrigin: aPoint clipRect: aRectangle [
	^ self copy
		setOrigin: aPoint
		clipRect: (self clipRect intersect: aRectangle ifNone: [0@0 corner: 0@0])
]

{ #category : #private }
AthensCanvasWrapper >> createClosedEllipsePathIn: aRectangle [
	| radius width |
	aRectangle area > 0
		ifFalse: [ ^ self ].
	width := aRectangle width.
	radius := width / 2.
	^ self canvas
		createPath: [ :builder |
			builder
				absolute;
				moveTo: width @ radius;
				cwArcTo: 0 @ radius angle: Float pi;
				cwArcTo: width @ radius angle: Float pi ]
]

{ #category : #private }
AthensCanvasWrapper >> createClosedPolygonPathFrom: aPointsArray [
	^ self canvas
		createPath: [ :builder |
			aPointsArray ifEmpty: [ builder ].
			builder absolute.
			builder moveTo: aPointsArray first.
			aPointsArray allButFirstDo: [ :p | builder lineTo: p ].
			builder close.
			builder ]
]

{ #category : #'canvas accessing' }
AthensCanvasWrapper >> depth [
	^ 32
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> draw: anObject [
	^anObject drawOn: self
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> drawImage: aForm at: aPoint [
	"Draw the given Form, which is assumed to be a Form or ColorForm"

	self drawImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> drawImage: aForm at: aPoint sourceRect: sourceRect [
	"Draw the given form."

	^self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form over
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> drawMorph: aMorph [
	(self isVisible: aMorph fullBounds)
		ifFalse: [ ^ self ].
	self draw: aMorph
]

{ #category : #'canvas drawing - polygons' }
AthensCanvasWrapper >> drawPolygon: vertices color: aColor borderWidth: aBorderWidth borderColor: aBorderColor [
	| path |
	self canvas pathTransform
		restoreAfter: [
			self canvas pathTransform.
			path := self createClosedPolygonPathFrom: vertices ].
	self canvas setPaint: aColor.
	self canvas drawShape: path.
	(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
	self canvas drawShape: path
]

{ #category : #'canvas drawing - polygons' }
AthensCanvasWrapper >> drawPolygon: vertices fillStyle: aFillStyle [
	| path |
	self canvas pathTransform
		restoreAfter: [
			self canvas pathTransform.
			path := self createClosedPolygonPathFrom: vertices ].
	self canvas setPaint: aFillStyle.
	self canvas drawShape: path
]

{ #category : #'canvas drawing - polygons' }
AthensCanvasWrapper >> drawPolygon: vertices fillStyle: aFillStyle borderWidth: aBorderWidth borderColor: aBorderColor [
	self drawPolygon: vertices color:  aFillStyle borderWidth:  aBorderWidth borderColor: aBorderColor
]

{ #category : #'canvas drawing - text' }
AthensCanvasWrapper >> drawString: aString at: aPoint font: aFontOrNil color: aColor [
	self
		drawString: aString
		in: (origin + aPoint extent: self clipRect extent)
		font: aFontOrNil
		color: aColor
]

{ #category : #'canvas drawing - text' }
AthensCanvasWrapper >> drawString: aString from: firstIndex to: lastIndex in: bounds font: aFontOrNil color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	| font |
	font := aFontOrNil ifNil: [ TextStyle defaultFont ].
	self canvasClipBy: (bounds intersect: self canvas clipRect) during:[
			self canvas pathTransform restoreAfter:[ | advance |
			self canvas pathTransform translateBy: bounds origin.
			self canvas pathTransform translateX: 0 Y: font getPreciseAscent.
			self flag: #pharoTodo. "scaled, so that FT-Plugin and Athens don't mix cached faces"
			self canvas pathTransform scaleBy: 1.0000001 @ 1.0000001.
			self canvas setPaint: aColor.
			self canvas setFont: font.
			advance := self canvas drawString: aString asString from: 1 to: aString size.
			underline ifTrue:[
				self canvas setPaint: uc.
				self canvas drawShape:(0@1 extent: advance x @1).
			]]]
]

{ #category : #'canvas drawing - text' }
AthensCanvasWrapper >> drawString: aString in: bounds [
	self drawString: aString in: bounds font:nil color: Color black
]

{ #category : #'canvas drawing - text' }
AthensCanvasWrapper >> drawString: aString in: bounds font: aFontOrNil color: aColor [
	| font |
	font := aFontOrNil ifNil: [ TextStyle defaultFont ].
	self canvasClipBy: (bounds intersect: self canvas clipRect) during:[
			self canvas pathTransform restoreAfter:[
			self canvas pathTransform translateBy: bounds origin.
			self canvas pathTransform translateX: 0 Y: font getPreciseAscent.
			self flag: #pharoTodo. "scaled, so that FT-Plugin and Athens don't mix cached faces"
			self canvas pathTransform scaleBy: 1.0000001 @ 1.0000001.
			self canvas setPaint: aColor.
			self canvas setFont: font.
			self canvas drawString: aString from: 1 to: aString size ] ]
]

{ #category : #'canvas drawing - text' }
AthensCanvasWrapper >> drawString: s in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	^ self
		drawString: s asString
		from: 1
		to: s size
		in: boundsRect
		font: fontOrNil
		color: c
		underline: underline
		underlineColor: uc
		strikethrough: strikethrough
		strikethroughColor: sc
]

{ #category : #'canvas accessing' }
AthensCanvasWrapper >> extent [
	^ self canvas surface extent
]

{ #category : #'canvas drawing - ovals' }
AthensCanvasWrapper >> fillOval: r color: c [

	self fillOval: r color: c borderWidth: 0 borderColor: Color transparent
]

{ #category : #'canvas drawing - ovals' }
AthensCanvasWrapper >> fillOval: aRectangle color: aColor borderWidth: aBorderWidth borderColor: aBorderColor [
	self
		fillOval: aRectangle
		fillStyle: aColor
		borderWidth: aBorderWidth
		borderColor: aBorderColor
]

{ #category : #'canvas drawing - ovals' }
AthensCanvasWrapper >> fillOval: aRectangle fillStyle: aFillStyle [
	^self fillOval: aRectangle fillStyle: aFillStyle borderWidth: 0 borderColor: Color transparent
]

{ #category : #'canvas drawing - ovals' }
AthensCanvasWrapper >> fillOval: aRectangle fillStyle: aFillStyle borderWidth: aBorderWidth borderColor: aBorderColor [
	self canvas setPaint: aFillStyle.
	self canvas pathTransform
		restoreAfter: [
			| path t |
			path := self createClosedEllipsePathIn: aRectangle.
			t := self canvas pathTransform.
			t translateBy: aRectangle origin.
			t scaleBy: 1 @ (aRectangle height / aRectangle width).
			self canvas setShape: path.
			self canvas draw.
			(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
			self canvas draw ]
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> fillRectangle: aRectangle color: aColor [
	self canvas setPaint: aColor.
	self canvas drawShape: aRectangle
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> fillRectangle: aRectangle fillStyle: aFillStyle [
	self fillRectangle: aRectangle color: aFillStyle
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> fillRectangle: aRectangle fillStyle: aFillStyle borderStyle: aBorderStyle [
	self
		canvasClipBy: self clipRect
		during: [
			self canvas setPaint: aFillStyle.
			self canvas drawShape: (aRectangle insetBy: aBorderStyle width).
			self canvas setStrokePaint: aBorderStyle.
			self canvas drawShape: (aRectangle insetBy: aBorderStyle width / 2 asFloat) ]
]

{ #category : #initialization }
AthensCanvasWrapper >> finish [
	self flush
]

{ #category : #'canvas drawing - support' }
AthensCanvasWrapper >> flush [
	^ self canvas surface flush
]

{ #category : #accessing }
AthensCanvasWrapper >> form [
	^ self canvas surface asForm
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> frameAndFillRectangle: aRectangle fillColor: aFillColor borderWidth: aBorderWidth borderColor: aBorderColor [
	self
		canvasClipBy: self clipRect
		during: [
			self canvas setPaint: aFillColor.
			self canvas drawShape: aRectangle.
			(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
			self canvas drawShape: aRectangle ]
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor [
	self fillRectangle: r fillStyle: fillColor.
	topLeftColor = bottomRightColor
		ifTrue: [ self frameRectangle: r width: borderWidth color: bottomRightColor ]
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> frameRectangle: aRectangle color: aColor [
	self frameRectangle: aRectangle width: 1 color: aColor
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> frameRectangle: aRectangle width: aBorderWidth color: aColor [
	(self canvas setStrokePaint: aColor) width: aBorderWidth.
	self canvas drawShape: aRectangle
]

{ #category : #'canvas drawing - rectangles' }
AthensCanvasWrapper >> frameRectangle: aRectangle width: width colors: colors dashes: dashes [
	| stroke |
	stroke := self canvas setStrokePaint: colors first.
	stroke width: width.
	stroke dashes: dashes offset: 0.
	self canvas drawShape: aRectangle.
	colors allButFirst
		ifNotEmpty: [ :e |
			stroke fillPaint: e first.
			stroke dashes: dashes reverse offset: dashes first.
			self canvas drawShape: aRectangle ]
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> fullDraw: anObject [
	^anObject fullDrawOn: self
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> fullDrawMorph: aMorph [
	"Hook method for potential other canvases. In the core, this method looks supefluous but PostscriptCanvases and other canvases can specialized it for Morph rendering. Therefore it should not be merged with fullDraw:."

	self fullDraw: aMorph
]

{ #category : #private }
AthensCanvasWrapper >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"This implementation may be wrong. If you have a bug it might be a good idea to dig a little here."

	self canvas pathTransform
		restoreAfter: [ self canvas pathTransform translateBy: aPoint.
			self canvas setPaint: aForm.
			self canvas drawShape: (0 @ 0 extent: sourceRect extent) ]
]

{ #category : #private }
AthensCanvasWrapper >> imagePaint: aForm at: aPoint sourceRect: sourceRect [
	self canvas pathTransform
		restoreAfter: [
			self canvas setPaint: aForm.
			self canvas pathTransform translateBy: aPoint.
			self canvas drawShape: sourceRect]
]

{ #category : #initialization }
AthensCanvasWrapper >> initialize [
	super initialize.
	origin := 0 @ 0
]

{ #category : #'canvas testing' }
AthensCanvasWrapper >> isVisible: aRectangle [
	"Return true if the given rectangle is (partially) visible"

	^ self clipRect intersects: aRectangle
]

{ #category : #'canvas drawing' }
AthensCanvasWrapper >> line: pt1 to: pt2 color: c [
	self
		line: pt1
		to: pt2
		width: 1
		color: c
]

{ #category : #'canvas drawing' }
AthensCanvasWrapper >> line: pt1 to: pt2 width: w color: c [
	| path |
	path := self createClosedPolygonPathFrom: {pt1 . pt2}.

	(self canvas setStrokePaint: c) width:w.
	self canvas drawShape: path
]

{ #category : #'canvas accessing' }
AthensCanvasWrapper >> origin [
	^ origin
]

{ #category : #private }
AthensCanvasWrapper >> origin: aPoint [
	origin := aPoint
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> paintImage: aForm at: aPoint [
	self paintImage: aForm at: aPoint sourceRect: aForm boundingBox
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> paintImage: aForm at: aPoint sourceRect: sourceRect [
	self canvas paintMode over.
	self imagePaint: aForm at: aPoint sourceRect: sourceRect
]

{ #category : #'canvas drawing' }
AthensCanvasWrapper >> paragraph: aParagraph bounds: aRectangle color: aColor [
	aParagraph drawOnAthensCanvas: self canvas bounds: aRectangle color: aColor
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> roundCornersOf: aMorph during: aBlock [
	^ self roundCornersOf: aMorph in: aMorph bounds during: aBlock
]

{ #category : #'canvas drawing - general' }
AthensCanvasWrapper >> roundCornersOf: aMorph in: bounds during: aBlock [
	^ aBlock value
]

{ #category : #'formcanvas copying' }
AthensCanvasWrapper >> rubParagraph: para bounds: bounds color: c [
	self paragraph: para bounds: bounds color: c
]

{ #category : #'formcanvas copying' }
AthensCanvasWrapper >> setOrigin: aPoint clipRect: aRectangle [
	origin := aPoint.
	currentClipRect := aRectangle
]

{ #category : #'canvas drawing' }
AthensCanvasWrapper >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [
	|mask|
	mask := stencilForm asFormOfDepth: 32.
	mask mapColor:Color white to: Color transparent.
	mask:= mask asAthensPaintOn: self canvas.
	self canvas pathTransform restoreAfter:[
		self canvas pathTransform translateBy: aPoint.
		self canvas setPaint: aColor.
		self canvas draw.
		mask maskOn: self canvas.
		]
]

{ #category : #'canvas drawing - support' }
AthensCanvasWrapper >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [
	self canvas
		transformBy: aDisplayTransform
		withClip: aClipRect
		in: aClipRect
		during: [ :v | aBlock value: self ]
]

{ #category : #'canvas drawing - support' }
AthensCanvasWrapper >> translateBy: offset clippingTo: aRect during: aBlock [
	self
		canvasClipBy: aRect
		during: [
			self canvas pathTransform
				restoreAfter: [
					self canvas pathTransform translateBy: offset.
					aBlock value: self ] ]
]

{ #category : #'canvas drawing - support' }
AthensCanvasWrapper >> translateBy: offset during: aBlock [
	^ self copyOrigin: origin + offset clipRect: self clipRect
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> translucentImage: aForm at: aPoint [
	self translucentImage: aForm at: aPoint sourceRect: aForm boundingBox
]

{ #category : #'canvas drawing - images' }
AthensCanvasWrapper >> translucentImage: aForm at: aPoint sourceRect: sourceRect [
	self canvas paintMode over.
	self imagePaint: aForm at: aPoint sourceRect: sourceRect
]
