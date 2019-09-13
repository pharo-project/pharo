"
Note that when shadowDrawing is true, shadowStipple may be either a color, for a solid shadow of the given color, or it may be a stipple used to simulate gray shading when the display cannot support alpha blending.
"
Class {
	#name : #FormCanvas,
	#superclass : #Canvas,
	#instVars : [
		'origin',
		'clipRect',
		'form',
		'port'
	],
	#classVars : [
		'TranslucentPatterns'
	],
	#category : #'Graphics-Canvas'
}

{ #category : #caching }
FormCanvas class >> defaultTranslucentPatterns [
	| patterns |
	patterns := Array new: 8.
	#(1 2 4 8)
		do: [ :d | 
			| mask bits pattern patternList |
			patternList := Array new: 5.
			mask := (1 bitShift: d) - 1.
			bits := 2 * d.
			[ bits >= 32 ]
				whileFalse: [ mask := mask bitOr: (mask bitShift: bits).	"double the length of mask"
					bits := bits + bits ].

			"0% pattern"
			pattern := Bitmap with: 0 with: 0.
			patternList at: 1 put: pattern.

			"25% pattern"
			pattern := Bitmap with: mask with: 0.
			patternList at: 2 put: pattern.

			"50% pattern"
			pattern := Bitmap with: mask with: mask bitInvert32.
			patternList at: 3 put: pattern.

			"75% pattern"
			pattern := Bitmap with: mask with: 4294967295.
			patternList at: 4 put: pattern.

			"100% pattern"
			pattern := Bitmap with: 4294967295 with: 4294967295.
			patternList at: 5 put: pattern.
			patterns at: d put: patternList ].

	^ patterns
]

{ #category : #'instance creation' }
FormCanvas class >> extent: aPoint [

	^ self extent: aPoint depth: Display depth

]

{ #category : #'instance creation' }
FormCanvas class >> extent: extent depth: depth [

	^ self new setForm: (Form extent: extent depth: depth)
]

{ #category : #'instance creation' }
FormCanvas class >> extent: extent depth: depth origin: aPoint clipRect: aRectangle [

	^ self new
		setForm: (Form extent: extent depth: depth);
		setOrigin: aPoint clipRect: aRectangle;
		yourself
]

{ #category : #'instance creation' }
FormCanvas class >> on: aForm [

	^ self new setForm: aForm

]

{ #category : #caching }
FormCanvas class >> translucentPatterns [

	^ TranslucentPatterns ifNil: [ TranslucentPatterns := self defaultTranslucentPatterns ]
]

{ #category : #accessing }
FormCanvas >> allocateForm: extentPoint [
	"Allocate a new form which is similar to the receiver"
	^form allocateForm: extentPoint
]

{ #category : #'drawing-ovals' }
FormCanvas >> balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [

	self asBalloonCanvas
		fillOval: aRectangle
		fillStyle: aFillStyle
		borderWidth: bw
		borderColor: bc
]

{ #category : #'Morphic-Base-Balloon' }
FormCanvas >> balloonFillRectangle: aRectangle fillStyle: aFillStyle [

	self asBalloonCanvas fillRectangle: aRectangle basicFillStyle: aFillStyle
]

{ #category : #'drawing-support' }
FormCanvas >> clipBy: aRectangle during: aBlock [
	"Set a clipping rectangle active only during the execution of aBlock.
	Note: In the future we may want to have more general clip shapes - not just rectangles"
	^aBlock value: (self copyClipRect: aRectangle)
]

{ #category : #accessing }
FormCanvas >> clipRect [
	"Return the currently active clipping rectangle"
	^ clipRect translateBy: origin negated
]

{ #category : #accessing }
FormCanvas >> contentsOfArea: aRectangle into: aForm [
	| bb |
	self flush.
	bb := BitBlt toForm: aForm.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: (aRectangle left + origin x); sourceY: (aRectangle top + origin y);
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm
]

{ #category : #copying }
FormCanvas >> copyClipRect: aRectangle [
	^ self copyOrigin: origin clipRect: (aRectangle translateBy: origin)

]

{ #category : #copying }
FormCanvas >> copyOffset: aPoint [
	^ self copyOrigin: origin + aPoint clipRect: clipRect
]

{ #category : #copying }
FormCanvas >> copyOffset: aPoint clipRect: sourceClip [
	"Make a copy of me offset by aPoint, and further clipped
	by sourceClip, a rectangle in the un-offset coordinates"
	^ self copyOrigin: aPoint + origin
		clipRect: ((sourceClip translateBy: origin) intersect: clipRect ifNone: [ 0@0 corner: 0@0 ])
]

{ #category : #copying }
FormCanvas >> copyOrigin: aPoint clipRect: aRectangle [
	"Return a copy of this canvas with the given origin. The clipping rectangle of this canvas is the intersection of the given rectangle and the receiver's current clipping rectangle. This allows the clipping rectangles of nested clipping morphs to be composed."
	^ self copy
		setOrigin: aPoint
		clipRect: (clipRect intersect: aRectangle ifNone: ["well, now we will clip everything" 0@0 corner: 0@0])
]

{ #category : #accessing }
FormCanvas >> depth [

	^ form depth

]

{ #category : #'drawing-polygons' }
FormCanvas >> drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc [
	"Generalize for the BalloonCanvas"
	^self drawPolygon: vertices fillStyle: aColor borderWidth: bw borderColor: bc
]

{ #category : #'drawing-polygons' }
FormCanvas >> drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Use a BalloonCanvas"
	self asBalloonCanvas 
		drawPolygon: vertices asArray
		fillStyle: aFillStyle
		borderWidth: bw 
		borderColor: bc
]

{ #category : #'drawing-text' }
FormCanvas >> drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c [
	| font |
	port colorMap: nil.
	font := fontOrNil ifNil: [TextStyle defaultFont].
	port combinationRule: Form paint.
	font installOn: port
		foregroundColor: c
		backgroundColor: Color transparent.
	font displayString: aString on: port 
		from: firstIndex to: lastIndex at: (origin + aPoint) kern: 0.
]

{ #category : #'drawing-text' }
FormCanvas >> drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c [
	| font portRect |
	port colorMap: nil.
	portRect := port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	font := fontOrNil ifNil: [TextStyle defaultFont].
	port combinationRule: Form paint.
	font installOn: port
		foregroundColor: c
		backgroundColor: Color transparent.
	font displayString: aString asString on: port 
		from: firstIndex to: lastIndex at: (bounds topLeft + origin) kern: 0.
	port clipRect: portRect.
]

{ #category : #'drawing-text' }
FormCanvas >> drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	| font portRect endPoint |
	port colorMap: nil.
	portRect := port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	font := fontOrNil ifNil: [TextStyle defaultFont].
	port combinationRule: Form paint.
	font installOn: port
		foregroundColor: c
		backgroundColor: Color transparent.
	endPoint := font displayString: aString asString on: port 
		from: firstIndex to: lastIndex at: (bounds topLeft + origin) kern: 0.
	underline ifTrue:[
		font installOn: port
			foregroundColor: uc
			backgroundColor: Color transparent;
			displayUnderlineOn: port from: (bounds topLeft + origin + (0@font ascent)) to: endPoint.	
		].
	strikethrough ifTrue: [ 
		font installOn: port
			foregroundColor: uc
			backgroundColor: Color transparent;
			displayStrikeoutOn: port from: (bounds topLeft + origin + (0@font ascent)) to: endPoint.	
	].

	port clipRect: portRect.
]

{ #category : #accessing }
FormCanvas >> extent [

	^ form extent
]

{ #category : #drawing }
FormCanvas >> fillColor: c [
	"Note: This always fills, even if the color is transparent."
	self setClearColor: c.
	port fillRect: form boundingBox offset: origin.
]

{ #category : #'drawing-ovals' }
FormCanvas >> fillOval: r color: fillColor borderWidth: borderWidth borderColor: borderColor [
	| rect |
	"draw the border of the oval"
	rect := (r translateBy: origin) truncated.
	(borderWidth = 0 or: [borderColor isTransparent]) ifFalse:[
		self setFillColor: borderColor.
		(r area > 10000 or: [fillColor isTranslucent]) 
			ifTrue: [port frameOval: rect borderWidth: borderWidth]
			ifFalse: [port fillOval: rect]]. "faster this way"
	"fill the inside"
	fillColor isTransparent ifFalse:
		[self setFillColor: fillColor.
		port fillOval: (rect insetBy: borderWidth)].

]

{ #category : #'drawing-ovals' }
FormCanvas >> fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Fill the given oval."

	self flag: #pharoFixMe.		"Bob: this and its siblings could be moved up to Canvas with the
						right #balloonFillOval:..."

	(aFillStyle isBitmapFill and:[aFillStyle isKindOf: InfiniteForm]) ifTrue:[
		self flag: #pharoFixMe. "See PluggableCanvas>>fillOval:fillStyle:borderWidth:borderColor:"
		^self fillOval: aRectangle color: aFillStyle borderWidth: bw borderColor: bc].
	(aFillStyle isSolidFill) ifTrue:[
		^self fillOval: aRectangle color: aFillStyle asColor borderWidth: bw borderColor: bc].
	"Use a BalloonCanvas instead"
	self balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc
]

{ #category : #'drawing-rectangles' }
FormCanvas >> fillRectangle: aRectangle basicFillStyle: aFillStyle [
	"Fill the given rectangle with the given, non-composite, fill style."
	
	| pattern |

	(aFillStyle isKindOf: InfiniteForm) ifTrue: [
		^self infiniteFillRectangle: aRectangle fillStyle: aFillStyle
	].

	(aFillStyle isSolidFill) 
		ifTrue:[^self fillRectangle: aRectangle color: aFillStyle asColor].
	"We have a very special case for filling with infinite forms"
	(aFillStyle isBitmapFill and:[aFillStyle origin = (0@0)]) ifTrue:[
		pattern := aFillStyle form.
		(aFillStyle direction = (pattern width @ 0) 
			and:[aFillStyle normal = (0@pattern height)]) ifTrue:[
				"Can use an InfiniteForm"
				^self fillRectangle: aRectangle color: (InfiniteForm with: pattern)].
	].
	"Use a BalloonCanvas instead"
	self balloonFillRectangle: aRectangle fillStyle: aFillStyle.
]

{ #category : #initialization }
FormCanvas >> finish [
	"If there are any pending operations on the receiver complete them. Do not return before all modifications have taken effect."
	form finish
]

{ #category : #other }
FormCanvas >> flushDisplay [
		Display deferUpdates: false; forceDisplayUpdate.
]

{ #category : #other }
FormCanvas >> forceToScreen: rect [
	^Display forceToScreen: rect.

]

{ #category : #accessing }
FormCanvas >> form [

	^ form
]

{ #category : #'drawing-rectangles' }
FormCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [
	| rect |
	rect := r translateBy: origin.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setFillColor: borderColor.
		(r area > 10000 or: [fillColor isTranslucent]) ifTrue: [
			port frameRect: rect borderWidth: borderWidth.
		] ifFalse: ["for small rectangles, it's faster to fill the entire outer rectangle
					than to compute and fill the border rects"
					port fillRect: rect offset: origin]].

	"fill the inside"
	fillColor isTransparent ifFalse:
		[self setFillColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) offset: origin].
]

{ #category : #'drawing-rectangles' }
FormCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor [

	| w h rect |
	"First use quick code for top and left borders and fill"
	self frameAndFillRectangle: r
		fillColor: fillColor
		borderWidth: borderWidth
		borderColor: topLeftColor.

	"Now use slow code for bevelled bottom and right borders"
	bottomRightColor isTransparent ifFalse: [
		borderWidth isNumber
			ifTrue: [w := h := borderWidth]
			ifFalse: [w := borderWidth x.   h := borderWidth y].
		rect := r translateBy: origin.
		self setFillColor: bottomRightColor.
		port 
			 frameRectRight: rect width: w;
			 frameRectBottom: rect height: h].

]

{ #category : #private }
FormCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [ 
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: aPoint + origin sourceRect: sourceRect rule: rule.
]

{ #category : #private }
FormCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule alpha: sourceAlpha [
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: aPoint + origin sourceRect: sourceRect rule: rule alpha: sourceAlpha.
]

{ #category : #'drawing-rectangles' }
FormCanvas >> infiniteFillRectangle: aRectangle fillStyle: aFillStyle [

	| additionalOffset rInPortTerms clippedPort targetTopLeft clipOffset ex |

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset := 0@0.
	ex := aFillStyle form extent.
	rInPortTerms := (aRectangle intersect: aFillStyle boundingBox ifNone: ["nothing to draw" ^ self ]) translateBy: origin.
	clippedPort := port clippedBy: rInPortTerms.
	targetTopLeft := clippedPort clipRect topLeft truncateTo: ex.
	clipOffset := rInPortTerms topLeft - targetTopLeft.
	additionalOffset := (clipOffset \\ ex) - ex.
	^aFillStyle
		displayOnPort: clippedPort
		offsetBy: additionalOffset

]

{ #category : #testing }
FormCanvas >> isVisible: aRectangle [
	"Optimization"
	(aRectangle right + origin x) < clipRect left	ifTrue: [^ false].
	(aRectangle left + origin x) > clipRect right	ifTrue: [^ false].
	(aRectangle bottom + origin y) < clipRect top	ifTrue: [^ false].
	(aRectangle top + origin y) > clipRect bottom	ifTrue: [^ false].
	^ true

]

{ #category : #drawing }
FormCanvas >> line: pt1 to: pt2 width: w color: c [
	| offset |
	offset := origin - (w // 2) asPoint.
	self setFillColor: c.
	port width: w; height: w;
		drawFrom: (pt1 + offset) to: (pt2 + offset)
]

{ #category : #accessing }
FormCanvas >> origin [
	"Return the current origin for drawing operations"
	^ origin
]

{ #category : #drawing }
FormCanvas >> paragraph: para bounds: bounds color: c [

	| scanner |
	self setPaintColor: c.
	scanner := (port clippedBy: (bounds translateBy: origin)) displayScannerFor: para
		foreground: c background: Color transparent
		ignoreColorChanges: false.
	para displayOn: (self copyClipRect: bounds) using: scanner at: origin+ bounds topLeft.

]

{ #category : #copying }
FormCanvas >> postCopy [
	"The copy share same underlying Form but with its own grafPort."

	super postCopy.
	self resetGrafPort
]

{ #category : #printing }
FormCanvas >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll:' on: '; print: form.
]

{ #category : #private }
FormCanvas >> privateClipRect [

	^clipRect
]

{ #category : #private }
FormCanvas >> privatePort [

	^port
]

{ #category : #private }
FormCanvas >> privateWarp: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize [
	"Warp the given using the appropriate transform and offset."
	| globalRect sourceQuad warp tfm |
	tfm := aTransform.
	globalRect := tfm localBoundsToGlobal: sourceRect.
	sourceQuad := (tfm sourceQuadFor: globalRect) collect:[:p| p - sourceRect topLeft].
	extraOffset ifNotNil:[globalRect := globalRect translateBy: extraOffset].
     warp := (WarpBlt toForm: port destForm)
                combinationRule: Form paint;
                sourceQuad: sourceQuad destRect: (globalRect origin corner: globalRect corner+(1@1));
                clipRect: port clipRect.
	warp cellSize: cellSize.
	warp sourceForm: aForm.
	warp warpBits
]

{ #category : #drawing }
FormCanvas >> render: anObject [
	"Do some 3D operations with the object if possible"
	^self asBalloonCanvas render: anObject
]

{ #category : #initialization }
FormCanvas >> reset [

	origin := 0@0.							"origin of the top-left corner of this cavas"
	clipRect := (0@0 corner: 10000@10000).		"default clipping rectangle"
	self shadowColor: nil.
]

{ #category : #private }
FormCanvas >> resetGrafPort [
	"Private! Create a new grafPort for a new copy."

	port := GrafPort toForm: form.
	port clipRect: clipRect.

]

{ #category : #private }
FormCanvas >> setClearColor: aColor [
	"Install a new clear color - e.g., a color is used for clearing the background"
	| clearColor |
	clearColor := aColor ifNil:[Color transparent].
	clearColor isColor ifFalse:[
		(clearColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].
		^port fillPattern: clearColor; combinationRule: Form over].
	"Okay, so clearColor really *is* a color"
	port sourceForm: nil.
	port combinationRule: Form over.
	port fillPattern: clearColor.
	self depth = 8 ifTrue:[
		"Use a stipple pattern"
		port fillColor: (form balancedPatternFor: clearColor)].

]

{ #category : #private }
FormCanvas >> setFillColor: aColor [
	"Install a new color used for filling."
	| screen patternWord fillColor |
	fillColor := aColor.
	fillColor ifNil:[fillColor := Color transparent].
	fillColor isColor ifFalse:[
		(fillColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].
		^port fillPattern: fillColor; combinationRule: Form over].
	"Okay, so fillColor really *is* a color"
	port sourceForm: nil.
	fillColor isTranslucent ifFalse:[
		port combinationRule: Form over.
		port fillPattern: fillColor.
		self depth = 8 ifTrue:[
			"In 8 bit depth it's usually a good idea to use a stipple pattern"
			port fillColor: (form balancedPatternFor: fillColor)].
		^self].
	"fillColor is some translucent color"

	self depth > 8 ifTrue:[
		"BitBlt setup for alpha masked transfer"
		port fillPattern: fillColor.
		self depth = 16
			ifTrue:[port alphaBits: fillColor privateAlpha; combinationRule: 30]
			ifFalse:[port combinationRule: Form blend].
		^self].
	"Can't represent actual transparency -- use stipple pattern"
	screen := self translucentMaskFor: fillColor alpha depth: self depth.
	patternWord := form pixelWordFor: fillColor.
	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).
	port combinationRule: Form paint.

]

{ #category : #private }
FormCanvas >> setForm: aForm [

	self reset.
	form := aForm.
	port := GrafPort toForm: form.

]

{ #category : #private }
FormCanvas >> setOrigin: aPoint clipRect: aRectangle [

	origin := aPoint.
	clipRect := aRectangle.
	port clipRect: aRectangle.

]

{ #category : #private }
FormCanvas >> setPaintColor: aColor [
	"Install a new color used for filling."
	
	| paintColor screen patternWord |
	paintColor := aColor.
	paintColor ifNil: [paintColor := Color transparent].
	paintColor isColor ifFalse: [ 
		(paintColor isKindOf: InfiniteForm) 
			ifFalse: [^self error:'Cannot install color'].
		^port fillPattern: paintColor; combinationRule: Form paint].
	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isTranslucent) ifFalse: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		self depth = 8 ifTrue: [
			port fillColor: (form balancedPatternFor: paintColor)].
		^self].
	"paintColor is translucent color"

	self depth > 8 ifTrue: [
		"BitBlt setup for alpha mapped transfer"
		port fillPattern: paintColor.
		self depth = 16
			ifTrue: [port alphaBits: paintColor privateAlpha; combinationRule: 31]
			ifFalse: [port combinationRule: Form blend].
		^self].

	"Can't represent actual transparency -- use stipple pattern"
	screen := self translucentMaskFor: paintColor alpha depth: self depth.
	patternWord := form pixelWordFor: paintColor.
	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).
	port combinationRule: Form paint

]

{ #category : #other }
FormCanvas >> showAt: pt [

	^ form displayAt: pt
]

{ #category : #other }
FormCanvas >> showAt: pt invalidRects: updateRects [
	| blt |
	blt := (BitBlt toForm: Display)
			sourceForm: form;
			combinationRule: Form over.
	updateRects do:
		[:rect |
		blt sourceRect: rect;
			destOrigin: rect topLeft + pt;
			copyBits]
]

{ #category : #'drawing-images' }
FormCanvas >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: aPoint + origin
		sourceRect: sourceRect.
]

{ #category : #'drawing-support' }
FormCanvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock	 smoothing: cellSize [

	"Note: This method has been originally copied from TransformationMorph."
	| innerRect patchRect sourceQuad warp start subCanvas rule|
	(aDisplayTransform isPureTranslation) ifTrue:[
		^aBlock value: (self copyOffset: aDisplayTransform offset negated truncated
							clipRect: aClipRect)
	].
	"Prepare an appropriate warp from patch to innerRect"
	innerRect := aClipRect.
	patchRect := (aDisplayTransform globalBoundsToLocal: innerRect) truncated.
	sourceQuad := (aDisplayTransform sourceQuadFor: innerRect)
					collect: [:p | p - patchRect topLeft].
	warp := self warpFrom: sourceQuad toRect: innerRect.
	warp cellSize: cellSize.

	"Render the submorphs visible in the clipping rectangle, as patchForm"
	start := (self depth = 1)
		"If this is true B&W, then we need a first pass for erasure."
		ifTrue: [1] ifFalse: [2].
	"If my depth has alpha, do blending rather than paint"
	rule := self depth = 32 ifTrue: [Form blend] ifFalse: [Form paint].
	start to: 2 do:
		[:i | "If i=1 we first make a shadow and erase it for opaque whites in B&W"
		subCanvas := self class extent: patchRect extent depth: self depth.
		i=1	ifTrue: [ warp combinationRule: Form erase ]
			ifFalse: [ warp combinationRule: rule].
		subCanvas translateBy: patchRect topLeft negated
			during:[:offsetCanvas| aBlock value: offsetCanvas].
		warp sourceForm: subCanvas form; warpBits.
		warp sourceForm: nil.  subCanvas := nil "release space for next loop"]

]

{ #category : #'drawing-support' }
FormCanvas >> translateBy: delta during: aBlock [
	"Set a translation only during the execution of aBlock."
	^aBlock value: (self copyOffset: delta)
]

{ #category : #'drawing-support' }
FormCanvas >> translateTo: newOrigin clippingTo: aRectangle during: aBlock [
	"Set a new origin and clipping rectangle only during the execution of aBlock."
	aBlock value: (self copyOrigin: newOrigin clipRect: aRectangle)
]

{ #category : #private }
FormCanvas >> translucentMaskFor: alphaValue depth: d [
	"Return a pattern representing a mask usable for stipple transparency"
	^(self translucentPatterns at: d) at: ((alphaValue min: 1.0 max: 0.0) * 4) rounded + 1 
]

{ #category : #private }
FormCanvas >> translucentPatterns [
	^ self class translucentPatterns
]

{ #category : #other }
FormCanvas >> warpFrom: sourceQuad toRect: destRect [
	^ (WarpBlt toForm: port destForm)
		combinationRule: Form paint;
		sourceQuad: sourceQuad destRect: (destRect translateBy: origin);
		clipRect: clipRect
]

{ #category : #'drawing-images' }
FormCanvas >> warpImage: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize [
	"Warp the given using the appropriate transform and offset."
	| tfm |
	tfm := (MatrixTransform2x3 withOffset: origin) composedWithLocal: aTransform.
	^self privateWarp: aForm transform: tfm at: extraOffset sourceRect: sourceRect cellSize: cellSize
]
