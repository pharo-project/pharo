"
- balloon engine using only first 4 variables of receiver instance:

 - work buffer
 - span
 - bitBlt (Bitblt object)
 - forms


"
Class {
	#name : #AthensBalloonEngine,
	#superclass : #BalloonEngine,
	#instVars : [
		'currentFill'
	],
	#category : #'Athens-Balloon-Engine'
}

{ #category : #copying }
AthensBalloonEngine >> copyBits [

	(bitBlt notNil and: [ bitBlt destForm notNil ]) ifTrue: [
		bitBlt destForm unhibernate ].
	self copyLoopFastest
]

{ #category : #drawing }
AthensBalloonEngine >> drawImage: aForm transform: m [
	| orig dir normal tr rect |

	rect := aForm boundingBox.
	orig := 0@0.
	dir := aForm width @ 0.
	normal := 0 @ aForm height.

	tr := edgeTransform.
	edgeTransform := m.

	self prepareForRendering.
	self registerFormFill: aForm
		origin: orig
		direction: dir
		normal: normal.
	self fillRectangle: rect.

	edgeTransform := tr
]

{ #category : #private }
AthensBalloonEngine >> fastReset [

	workBuffer ifNil: [ ^ self reset ].

	workBuffer
		at: (1+GWState) put: GEStateUnlocked;
		at: (1+GWObjUsed) put: 4;
		at: (1+GWNeedsFlush) put: 0;
		at: (1+GWCurrentZ) put: 0;
		at: (1+GWGETStart) put: 0";

		at: (1+GWGETUsed) put: 0;
		at: (1+GWAETStart) put: 0;
		at: (1+GWAETUsed) put: 0".

	self primSetEdgeTransform: edgeTransform.
	self primSetClipRect: clipRect.
	self primSetColorTransform: colorTransform.
	self primSetDepth: self primGetDepth + 1.

	postFlushNeeded := false.
	forms := #()

"
-	self magicNumberPut: GWMagicNumber.
- 	self wbSizePut: size.
+	self wbTopPut: size.
+	self statePut: GEStateUnlocked.
-	self objStartPut: GWHeaderSize.
+	self objUsedPut: 4.
-	self objectTypeOf: 0 put: GEPrimitiveFill.
-	self objectLengthOf: 0 put: 4.
-	self objectIndexOf: 0 put: 0.

	self getStartPut: 0.
	self getUsedPut: 0.
	self aetStartPut: 0.
	self aetUsedPut: 0.
	self stopReasonPut: 0.

+	self needsFlushPut: 0.
-	self clipMinXPut: 0.
-	self clipMaxXPut: 0.
-	self clipMinYPut: 0.
-	self clipMaxYPut: 0.
+	self currentZPut: 0.
	self resetGraphicsEngineStats.
	self initEdgeTransform.
	self initColorTransform.
"
]

{ #category : #drawing }
AthensBalloonEngine >> fillBezierShape: contours [

	contours do:[ :points |
		self primAddBezierShape: points
			segments: (points size // 3)
			fill: currentFill
			lineWidth: 0
			lineFill: 0.

		"Note: To avoid premature flushing of the pipeline we need to reset the flush bit within the engine."
		self primFlushNeeded: false.
	].

	self copyBits
]

{ #category : #drawing }
AthensBalloonEngine >> fillRectangle: rect [

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin
			to: rect corner
			fillIndex: currentFill
			borderWidth: 0
			borderColor: 0.

	self copyBits
]

{ #category : #private }
AthensBalloonEngine >> prepareForRendering [

	self fastReset
]

{ #category : #'registering fills' }
AthensBalloonEngine >> registerColorFill: aColor [

	currentFill := aColor scaledPixelValue32
]

{ #category : #'registering fills' }
AthensBalloonEngine >> registerFormFill: form origin: orig direction: dir normal: norm [

	self
		registerFormFill: form
		origin: orig
		direction: dir
		normal: norm
		tileFlag: true
]

{ #category : #'registering fills' }
AthensBalloonEngine >> registerFormFill: form origin: orig direction: dir normal: norm tileFlag: tileFlag [

	forms := forms copyWith: form.
	form unhibernate.
	currentFill := self primAddBitmapFill: form
			colormap: (form colormapIfNeededForDepth: 32)
			tile: tileFlag
			from: orig
			along: dir
			normal: norm
			xIndex: forms size
]

{ #category : #paints }
AthensBalloonEngine >> registerLinearGradient: colorRamp origin: origin corner: corner [

	| pixelRamp direction normal |
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.
	direction := corner - origin.
	normal := direction y negated @ direction x.

	currentFill := self
		               primAddGradientFill: pixelRamp
		               from: origin
		               along: direction
		               normal: normal
		               radial: false
]

{ #category : #paints }
AthensBalloonEngine >> registerRadialGradient: colorRamp center: aCenter direction: dir normal: n [

	| pixelRamp |
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.

	currentFill := self
		               primAddGradientFill: pixelRamp
		               from: aCenter
		               along: dir
		               normal: n
		               radial: true
]

{ #category : #paints }
AthensBalloonEngine >> registerRadialGradient: colorRamp center: aCenter radius: r [

	| pixelRamp |
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.

	currentFill := self
		               primAddGradientFill: pixelRamp
		               from: aCenter
		               along: aCenter x + r @ aCenter y
		               normal: aCenter x @ aCenter y - r
		               radial: true
]

{ #category : #initialization }
AthensBalloonEngine >> reset [

	workBuffer ifNil:[workBuffer := self class allocateOrRecycleBuffer: 10000].

	self
		primInitializeBuffer: workBuffer;
		primSetAALevel: 4;
		primSetOffset: 0@0;
		primSetClipRect: clipRect;
		primSetEdgeTransform: edgeTransform;
		primSetColorTransform: colorTransform.

	forms := #()
]

{ #category : #accessing }
AthensBalloonEngine >> setFill: aFill [

	currentFill := aFill
]

{ #category : #private }
AthensBalloonEngine >> strokeBezierShape: contours width: aWidth [

	contours do:[ :points |
		self primAddBezierShape: points
			segments: (points size // 3)
			fill: 0
			lineWidth: aWidth
			lineFill: currentFill.

		"Note: To avoid premature flushing of the pipeline we need to reset the flush bit within the engine."
		self primFlushNeeded: false.
	].

	self copyBits
]

{ #category : #drawing }
AthensBalloonEngine >> strokeRectangle: rect borderWidth: width [

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin
			to: rect corner
			fillIndex: 0
			borderWidth: width
			borderColor: currentFill.

	self copyBits
]

{ #category : #drawing }
AthensBalloonEngine >> strokeRectangle: rect width: aWidth [

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin
			to: rect corner
			fillIndex: 0
			borderWidth: aWidth asInteger
			borderColor: currentFill.

	self copyBits
]
