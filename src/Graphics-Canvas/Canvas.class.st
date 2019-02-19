"
A canvas is a two-dimensional medium on which morphs are drawn in a device-independent manner. Canvases keep track of the origin and clipping rectangle, as well as the underlying drawing medium (such as a window, pixmap, or postscript script).

Subclasses must implement (at least) the following methods:
	* Drawing:
		#fillOval:color:borderWidth:borderColor:
		#frameAndFillRectangle:fillColor:borderWidth:borderColor:
		#drawPolygon:color:borderWidth:borderColor:
		#image:at:sourceRect:rule:
		#stencil:at:sourceRect:rule:
		#line:to:width:color:
		#paragraph:bounds:color:
		#text:bounds:font:color:
	* Support
		#clipBy:during:
		#translateBy:during:
		#translateBy:clippingTo:during:
		#transformBy:clippingTo:during:

"
Class {
	#name : #Canvas,
	#superclass : #Object,
	#category : #'Graphics-Canvas'
}

{ #category : #converting }
Canvas >> asShadowDrawingCanvas [
	^self asShadowDrawingCanvas: (Color black alpha: 0.5).
]

{ #category : #converting }
Canvas >> asShadowDrawingCanvas: aColor [
	^(ShadowDrawingCanvas on: self) shadowColor: aColor
]

{ #category : #'drawing-support' }
Canvas >> cache: aRectangle using: aCache during: aBlock [ 
	"Cache the execution of aBlock by the given cache.
	Note: At some point we may want to actually *create* the cache here;
		for now we're only using it."

	(aCache notNil 
		and: [(aCache isForm) and: [aCache extent = aRectangle extent]]) 
			ifTrue: [^self paintImage: aCache at: aRectangle origin].
	aBlock value: self
]

{ #category : #'drawing-support' }
Canvas >> clipBy: aRectangle during: aBlock [
	"Set a clipping rectangle active only during the execution of aBlock.
	Note: In the future we may want to have more general clip shapes - not just rectangles"
	^self subclassResponsibility
]

{ #category : #accessing }
Canvas >> clipRect [
	"Return the currently active clipping rectangle"
	^self subclassResponsibility
]

{ #category : #accessing }
Canvas >> contentsOfArea: aRectangle [
	"Return the contents of the given area"
	^self contentsOfArea: aRectangle into: (Form extent: aRectangle extent depth: self depth)
]

{ #category : #accessing }
Canvas >> contentsOfArea: aRectangle into: aForm [
	"Return the contents of the given area"
	^self subclassResponsibility
]

{ #category : #accessing }
Canvas >> depth [

	^ Display depth

]

{ #category : #testing }
Canvas >> doesRoundedCorners [ 

	^ true
]

{ #category : #'drawing-general' }
Canvas >> draw: anObject [
	^anObject drawOn: self
]

{ #category : #'drawing-images' }
Canvas >> drawImage: aForm at: aPoint [
	"Draw the given Form, which is assumed to be a Form or ColorForm"

	self drawImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox
]

{ #category : #'drawing-images' }
Canvas >> drawImage: aForm at: aPoint sourceRect: sourceRect [
	"Draw the given form."

	^self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form over
]

{ #category : #'drawing-general' }
Canvas >> drawMorph: aMorph [
	"Draw the receiver morph on the receiver"
	
	"Changed to improve performance. Have seen a 30% improvement."	
	(aMorph fullBounds intersects: self clipRect)
		ifFalse: [^self].
	self draw: aMorph
]

{ #category : #'drawing-polygons' }
Canvas >> drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc [
	"Draw the given polygon."
	^self subclassResponsibility
]

{ #category : #'drawing-polygons' }
Canvas >> drawPolygon: vertices fillStyle: aFillStyle [
	"Fill the given polygon."
	self drawPolygon: vertices fillStyle: aFillStyle borderWidth: 0 borderColor: Color transparent
]

{ #category : #'drawing-polygons' }
Canvas >> drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Fill the given polygon.
	Note: The default implementation does not recognize any enhanced fill styles"
	self drawPolygon: vertices color: aFillStyle asColor borderWidth: bw borderColor: bc
]

{ #category : #'drawing-text' }
Canvas >> drawString: s at: pt [

	^ self drawString: s from: 1 to: s size at: pt font: nil color: Color black
]

{ #category : #'drawing-text' }
Canvas >> drawString: s at: pt font: aFont color: aColor [

	^ self drawString: s from: 1 to: s size at: pt font: aFont color: aColor
]

{ #category : #'drawing-text' }
Canvas >> drawString: s from: firstIndex to: lastIndex at: pt font: font color: aColor [
	self drawString: s from: firstIndex to: lastIndex in: (pt extent: 10000@10000) font: font color: aColor
]

{ #category : #'drawing-text' }
Canvas >> drawString: s from: firstIndex to: lastIndex at: pt font: font color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	self drawString: s from: firstIndex to: lastIndex in: (pt extent: 10000@10000) font: font color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
]

{ #category : #'drawing-text' }
Canvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [
	^self subclassResponsibility
]

{ #category : #'drawing-text' }
Canvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	^self subclassResponsibility
]

{ #category : #'drawing-text' }
Canvas >> drawString: s in: boundsRect [
	^self drawString: s from: 1 to: s size in: boundsRect font: nil color: Color black
]

{ #category : #'drawing-text' }
Canvas >> drawString: s in: boundsRect font: fontOrNil color: c [
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c
]

{ #category : #'drawing-text' }
Canvas >> drawString: s in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
]

{ #category : #accessing }
Canvas >> extent [
	"Return the physical extent of the output device"
	^self subclassResponsibility
]

{ #category : #drawing }
Canvas >> fillColor: aColor [
	"Fill the receiver with the given color.
	Note: This method should be named differently since it is intended to fill the background and thus fills even if the color is transparent"
	^self fillRectangle: self clipRect color: (aColor alpha: 1.0).
]

{ #category : #'drawing-ovals' }
Canvas >> fillOval: r color: c [

	self fillOval: r color: c borderWidth: 0 borderColor: Color transparent.

]

{ #category : #'drawing-ovals' }
Canvas >> fillOval: r color: c borderWidth: borderWidth borderColor: borderColor [
	"Fill the given oval."
	^self subclassResponsibility
]

{ #category : #'drawing-ovals' }
Canvas >> fillOval: aRectangle fillStyle: aFillStyle [
	"Fill the given oval."
	^self fillOval: aRectangle fillStyle: aFillStyle borderWidth: 0 borderColor: Color transparent
]

{ #category : #'drawing-ovals' }
Canvas >> fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Fill the given oval.
	Note: The default implementation does not recognize any enhanced fill styles"
	self fillOval: aRectangle color: aFillStyle asColor borderWidth: bw borderColor: bc
]

{ #category : #'drawing-rectangles' }
Canvas >> fillRectangle: aRectangle basicFillStyle: aFillStyle [
	"Fill the given rectangle with the given, non-composite, fill style
	Note: The default implementation does not recognize any enhanced fill styles."
	
	self fillRectangle: aRectangle color: aFillStyle asColor.
]

{ #category : #'drawing-rectangles' }
Canvas >> fillRectangle: r color: c [
	"Fill the rectangle using the given color"
	^self 
		frameAndFillRectangle: r
		fillColor: c
		borderWidth: 0
		borderColor: Color transparent
]

{ #category : #'drawing-rectangles' }
Canvas >> fillRectangle: aRectangle fillStyle: aFillStyle [
	"Fill the given rectangle. Double-dispatched via the fill style."
	
	aFillStyle fillRectangle: aRectangle on: self
]

{ #category : #'drawing-rectangles' }
Canvas >> fillRectangle: aRectangle fillStyle: aFillStyle borderStyle: aBorderStyle [
	"Fill the given rectangle."
       aFillStyle isTransparent ifFalse: [
               self fillRectangle: (aRectangle insetBy: aBorderStyle width) fillStyle: aFillStyle ].
       
       (aBorderStyle notNil and: [aBorderStyle width]) >= 0 ifTrue: [ aBorderStyle frameRectangle: aRectangle on: self ].
       
]

{ #category : #initialization }
Canvas >> finish [
	"If there are any pending operations on the receiver complete them. Do not return before all modifications have taken effect."
	^self flush
]

{ #category : #initialization }
Canvas >> flush [
]

{ #category : #other }
Canvas >> flushDisplay [
	"Empty hook method."
]

{ #category : #other }
Canvas >> forceToScreen: rect [
	"Empty hook method"

]

{ #category : #accessing }
Canvas >> form [

	^ Display

]

{ #category : #'drawing-rectangles' }
Canvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [
	"Draw the rectangle using the given attributes"
	^self subclassResponsibility
]

{ #category : #'drawing-rectangles' }
Canvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor [
	"Draw the rectangle using the given attributes.
	Note: This is a *very* simple implementation"
	| bw pt |
	self frameAndFillRectangle: r
		fillColor: fillColor
		borderWidth: borderWidth
		borderColor: bottomRightColor.
	bottomRightColor = topLeftColor ifFalse: [
		bw := borderWidth asPoint.
		pt := r topLeft + (bw // 2).
		self line: pt to: pt + ((r extent x - bw x)@0) width: borderWidth color: topLeftColor.
		self line: pt to: pt + (0@(r extent y - bw y)) width: borderWidth color: topLeftColor.
	].
]

{ #category : #'drawing-ovals' }
Canvas >> frameOval: r color: c [

	self fillOval: r color: Color transparent borderWidth: 1 borderColor: c.

]

{ #category : #'drawing-ovals' }
Canvas >> frameOval: r width: w color: c [

	self fillOval: r color: Color transparent borderWidth: w borderColor: c.

]

{ #category : #'drawing-rectangles' }
Canvas >> frameRectangle: r color: c [

	self frameRectangle: r width: 1 color: c.

]

{ #category : #'drawing-rectangles' }
Canvas >> frameRectangle: r width: w color: c [
	"Draw a frame around the given rectangle"
	^self frameAndFillRectangle: r
			fillColor: Color transparent
			borderWidth: w
			borderColor: c
]

{ #category : #'drawing-rectangles' }
Canvas >> frameRectangle: aRectangle width: width colors: colors dashes: dashes [
	"Draw a rectangle with the given width, colors and dash lengths."

	self
		frameRectangle: aRectangle
		width: width
		colors: colors
		dashes: dashes
		offset: self origin
]

{ #category : #'drawing-rectangles' }
Canvas >> frameRectangle: aRectangle width: width colors: colors dashes: dashes offset: offset [
	"Draw a rectangle with the given width, colors and dash lengths.
	The offset specifies how the coordinate system is translated from the screen origin
	(infinite forms are 0@0 screen based)."

	|o s hf vf c r ds di d os l|
	width < 1 ifTrue: [^self].
	dashes size < 2
		ifTrue: [^self  frameRectangle: aRectangle width: width color: colors first].
	r := aRectangle.
	s := dashes sum * width.
	ds := dashes size.
	di := 1.
	d := (dashes at: di) * width.
	c := colors at: di.
	hf := Form extent: s @ 1 depth: 32.
	r height >= width ifTrue: [
		o := r left + offset x \\ s.
		0 to: s - 1 do: [:x |
			hf colorAt: x + o \\ s  @ 0 put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := 0.
		l := r width truncateTo: width.
		self
			fillRectangle: (r topLeft + (os@0) extent: l@width)
			color: (InfiniteForm with: hf)].
	vf := Form extent: 1 @ s depth: 32.
	r width >= width ifTrue: [
		o := r top + offset y + width + (s - (r width \\ s)) \\ s.
		0 to: s - 1 do: [:y |
			vf colorAt: 0 @ (y + o \\ s) put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := width - (r width \\ width).
		l := r height - os truncateTo: width.
		self
			fillRectangle: (r topRight + (width negated @ os) extent: width@l)
			color: (InfiniteForm with: vf)].
	r height > width ifTrue: [
		o := r right + offset x - (width * 2) + (r height \\ s) + (r width \\ s) \\ s.
		0 to: s - 1 do: [:x |
			hf colorAt: o + s -1 - x \\ s  @ 0 put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := width - (r width \\ width + (r height \\ width) \\ width).
		l := r width - os truncateTo: width.
		os := (r width - os) \\ width.
		self
			fillRectangle: (r bottomLeft + (os @ width negated) extent: l@width)
			color: (InfiniteForm with: hf)].
	r width > width ifTrue: [
		o :=  r top + offset y + (r height * 2 \\ s) + (r width * 2 \\ s) - (width * 3) \\ s.
		0 to: s - 1 do: [:y |
			vf colorAt: 0 @ (o + s -1 - y \\ s) put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		l := r height - (2 * width) + os.
		os := width.
		self
			fillRectangle: (r topLeft + (0@os) extent: width@l)
			color: (InfiniteForm with: vf)]
]

{ #category : #'drawing-general' }
Canvas >> fullDraw: anObject [
	^anObject fullDrawOn: self
]

{ #category : #'drawing-general' }
Canvas >> fullDrawMorph: aMorph [
	"Hook method for potential other canvases. In the core, this method looks supefluous but PostscriptCanvases and other canvases can specialized it for Morph rendering. Therefore it should not be merged with fullDraw:."

	self fullDraw: aMorph
]

{ #category : #private }
Canvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"Note: The public use of this protocol is deprecated. It will become private. Nobody in the outside world must assume that a thing like a combination rule has any specific effect."
	^self subclassResponsibility
]

{ #category : #private }
Canvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule alpha: sourceAlpha [
	"Privately used for blending forms w/ constant alpha. Fall back to simpler case by defaul."
	^self image: aForm at: aPoint sourceRect: sourceRect rule: rule
]

{ #category : #testing }
Canvas >> isBalloonCanvas [
	^false
]

{ #category : #testing }
Canvas >> isShadowDrawing [
	^false
]

{ #category : #testing }
Canvas >> isVisible: aRectangle [
	"Return true if the given rectangle is (partially) visible"
	^self clipRect intersects: aRectangle

]

{ #category : #drawing }
Canvas >> line: pt1 to: pt2 color: c [

	self line: pt1 to: pt2 width: 1 color: c.

]

{ #category : #drawing }
Canvas >> line: pt1 to: pt2 width: w color: c [
	"Draw a line using the given width and color"
	^self subclassResponsibility
]

{ #category : #drawing }
Canvas >> line: pt1 to: pt2 width: width color: color1 dashLength: s1 secondColor: color2 secondDashLength: s2 startingOffset: startingOffset [ 
	"Draw a line using the given width, colors and dash lengths.
	Originally written by Stephan Rudlof; tweaked by Dan Ingalls
	to use startingOffset for sliding offset as in 'ants' animations.
	Returns the sum of the starting offset and the length of this line."

	| dist deltaBig colors nextPhase segmentOffset phase segmentLength startPoint distDone endPoint segLens |
	dist := pt1 distanceTo: pt2.
	dist = 0 ifTrue: [^startingOffset].
	s1 = 0 & (s2 = 0) ifTrue: [^startingOffset].
	deltaBig := pt2 - pt1.
	colors := { 
				color1.
				color2}.
	segLens := { 
				s1 asFloat.
				s2 asFloat}.
	nextPhase := { 
				2.
				1}.

	"Figure out what phase we are in and how far, given startingOffset."
	segmentOffset := startingOffset \\ (s1 + s2).
	segmentLength := segmentOffset < s1 
		ifTrue: 
			[phase := 1.
			s1 - segmentOffset]
		ifFalse: 
			[phase := 2.
			 s1 + s2 - segmentOffset].
	startPoint := pt1.
	distDone := 0.0.
	[distDone < dist] whileTrue: 
			[segmentLength := segmentLength min: dist - distDone.
			endPoint := startPoint + (deltaBig * segmentLength / dist).
			self 
				line: startPoint truncated
				to: endPoint truncated
				width: width
				color: (colors at: phase).
			distDone := distDone + segmentLength.
			phase := nextPhase at: phase.
			startPoint := endPoint.
			segmentLength := segLens at: phase].
	^startingOffset + dist
]

{ #category : #drawing }
Canvas >> line: pt1 to: pt2 width: w1 color: c1 stepWidth: s1 secondWidth: w2 secondColor: c2 secondStepWidth: s2 [ 
	"Draw a line using the given width, colors and steps; both steps can  
	have different stepWidths (firstStep, secondStep), draw widths and  
	colors."
	| bigSteps offsetPoint dist p1p2Vec deltaBig delta1 delta2 lastPoint bigStep |
	s1 = 0 & (s2 = 0) ifTrue: [^ self].
	dist := pt1 distanceTo: pt2.
	dist = 0 ifTrue: [^ self].
	bigStep := s1 + s2.
	bigSteps := dist / bigStep.
	p1p2Vec := pt2 - pt1.
	deltaBig := p1p2Vec / bigSteps.
	delta1 := deltaBig * (s1 / bigStep).
	delta2 := deltaBig * (s2 / bigStep).
	dist <= s1
		ifTrue: 
			[self
				line: pt1 rounded
				to: pt2 rounded
				width: w1
				color: c1.
			^ self].
	0 to: bigSteps truncated - 1 do: 
		[:bigStepIx | 
		self
			line: (pt1 + (offsetPoint := deltaBig * bigStepIx)) rounded
			to: (pt1 + (offsetPoint := offsetPoint + delta1)) rounded
			width: w1
			color: c1.
		self
			line: (pt1 + offsetPoint) rounded
			to: (pt1 + (offsetPoint + delta2)) rounded
			width: w2
			color: c2].
	"if there was no loop, offsetPoint is nil"
	lastPoint := pt1 + ((offsetPoint ifNil: [0 @ 0])
					+ delta2).
	(lastPoint dist: pt2)
		<= s1
		ifTrue: [self
				line: lastPoint rounded
				to: pt2 rounded
				width: w1
				color: c1]
		ifFalse: 
			[self
				line: lastPoint rounded
				to: (lastPoint + delta1) rounded
				width: w1
				color: c1.
			self
				line: (lastPoint + delta1) rounded
				to: pt2
				width: w1
				color: c2]
]

{ #category : #drawing }
Canvas >> line: pt1 to: pt2 width: width colors: colors dashes: dashes startingOffset: startingOffset [ 
	"Draw a line using the given width, colors and dash lengths.
	Dash lengths are considered as multiples of width."

	|dist deltaBig segmentOffset phase segmentLength startPoint distDone endPoint segLens lens l ep|
	width = 0 ifTrue: [^startingOffset].
	dist := pt1 distanceTo: pt2.
	dist = 0 ifTrue: [^startingOffset].
	(dashes allSatisfy: [:d | d = 0]) ifTrue: [^startingOffset].
	deltaBig := pt2 - pt1.
	segLens := dashes collect: [:d | d * width].
	
	"Figure out what phase we are in and how far, given startingOffset."
	segmentOffset := startingOffset \\ segLens sum.
	lens := segLens readStream.
	l := 0.
	[lens atEnd or: [segmentOffset <= (l := l + lens next)]] whileFalse: [].
	segmentLength := lens atEnd
		ifTrue: [phase := segLens size.
				segLens sum - segmentOffset]
		ifFalse: [phase := lens position.
				(segLens first: phase) sum - segmentOffset.].
	startPoint := pt1.
	distDone := 0.0.
	segmentLength < (segLens at: phase)
		ifTrue: [startPoint := startPoint + (deltaBig * segmentLength / dist).
				distDone := distDone + segmentLength.
				phase := phase \\ segLens size + 1.
				segmentLength :=  (segLens at: phase)].
	[distDone < dist] whileTrue: 
			[segmentLength := segmentLength min: dist - distDone.
			endPoint := startPoint + (deltaBig * segmentLength / dist).
			ep := startPoint + (deltaBig * (segmentLength - width max: 0) / dist).
			self 
				line: startPoint truncated
				to: ep truncated
				width: width
				color: (colors at: phase).
			distDone := distDone + segmentLength.
			phase := phase \\ segLens size + 1.
			startPoint := endPoint.
			segmentLength := segLens at: phase].
	^startingOffset + distDone
]

{ #category : #accessing }
Canvas >> origin [
	"Return the current origin for drawing operations"
	^self subclassResponsibility
]

{ #category : #'drawing-images' }
Canvas >> paintImage: aForm at: aPoint [
	"Draw the given Form, which is assumed to be a Form or ColorForm following the convention that zero is the transparent pixel value."

	self paintImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox

]

{ #category : #'drawing-images' }
Canvas >> paintImage: aForm at: aPoint sourceRect: sourceRect [
	"Draw the given Form, which is assumed to be a Form or ColorForm following the convention that zero is the transparent pixel value."

	^self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form paint
]

{ #category : #drawing }
Canvas >> paragraph: paragraph bounds: bounds color: c [
	"Draw the given paragraph"
	^self subclassResponsibility
]

{ #category : #'drawing-support' }
Canvas >> preserveStateDuring: aBlock [
	"Preserve the full canvas state during the execution of aBlock"
	^aBlock value: self copy
]

{ #category : #drawing }
Canvas >> render: anObject [
	"Do some 3D operations with the object if possible"
]

{ #category : #'drawing-general' }
Canvas >> roundCornersOf: aMorph during: aBlock [
	^self roundCornersOf: aMorph in: aMorph bounds during: aBlock
]

{ #category : #'drawing-general' }
Canvas >> roundCornersOf: aMorph in: bounds during: aBlock [
	^aBlock value
]

{ #category : #'drawing-general' }
Canvas >> roundShadowCornersOf: aMorph during: aBlock [
	^self roundShadowCornersOf: aMorph in: aMorph bounds during: aBlock
]

{ #category : #'drawing-general' }
Canvas >> roundShadowCornersOf: aMorph in: bounds during: aBlock [
	^aBlock value
]

{ #category : #testing }
Canvas >> seesNothingOutside: aRectangle [
	"Return true if this canvas will not touch anything outside aRectangle"
	^ aRectangle containsRect: self clipRect

]

{ #category : #accessing }
Canvas >> shadowColor [
	"Return the current override color or nil if no such color exists"
	^nil
]

{ #category : #accessing }
Canvas >> shadowColor: aColor [
	"Set a shadow color. If set this color overrides any client-supplied color."
]

{ #category : #'drawing-images' }
Canvas >> stencil: stencilForm at: aPoint color: aColor [
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor
]

{ #category : #'drawing-images' }
Canvas >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self subclassResponsibility
]

{ #category : #'drawing-support' }
Canvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock [
	"Transform the receiver by the given display transformation during the execution of aBlock. The given clip rectangle defines the *global* (e.g., outer) rectangle against which the receiver should clip (which would be equivalent to 'self clipRect: aClipRect; transformBy: aDisplayTransform')."
	^ self transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: 1

]

{ #category : #'drawing-support' }
Canvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [
	"Transform the receiver by the given display transformation during the execution of aBlock. The given clip rectangle defines the *global* (e.g., outer) rectangle against which the receiver should clip (which would be equivalent to 'self clipRect: aClipRect; transformBy: aDisplayTransform')."
	^ self subclassResponsibility
]

{ #category : #other }
Canvas >> translateBy:aPoint clippingTo:aRect during:aBlock [
	^aBlock value:(self copyOffset:aPoint clipRect:aRect).
]

{ #category : #'drawing-support' }
Canvas >> translateBy: delta during: aBlock [
	"Set a translation only during the execution of aBlock."
	^self subclassResponsibility
]

{ #category : #'drawing-support' }
Canvas >> translateTo: newOrigin clippingTo: aRectangle during: aBlock [
	"Set a new origin and clipping rectangle only during the execution of aBlock."
	self translateBy: newOrigin - self origin 
		clippingTo: (aRectangle translateBy: self origin negated) 
		during: aBlock
]

{ #category : #'drawing-images' }
Canvas >> translucentImage: aForm at: aPoint [
	"Draw a translucent image using the best available way of representing translucency."
	self translucentImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox
]

{ #category : #'drawing-images' }
Canvas >> translucentImage: aForm at: aPoint sourceRect: sourceRect [
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	(self depth < 32 or:[aForm isTranslucent not]) 
		ifTrue:[^self paintImage: aForm at: aPoint sourceRect: sourceRect].
	self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form blend
]

{ #category : #'drawing-images' }
Canvas >> warpImage: aForm transform: aTransform [
	"Warp the given form using aTransform"
	^self warpImage: aForm transform: aTransform at: 0@0
]

{ #category : #'drawing-images' }
Canvas >> warpImage: aForm transform: aTransform at: extraOffset [
	"Warp the given form using aTransform.
	TODO: Use transform to figure out appropriate cell size"
	^self warpImage: aForm transform: aTransform at: extraOffset sourceRect: aForm boundingBox cellSize: 1
]

{ #category : #'drawing-images' }
Canvas >> warpImage: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize [
	"Warp the given using the appropriate transform and offset."
	^self subclassResponsibility
]
