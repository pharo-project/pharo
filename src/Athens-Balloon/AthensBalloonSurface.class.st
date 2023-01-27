"
I am concrete implementation of Athens surface which using balloon engine for rendering. 
"
Class {
	#name : #AthensBalloonSurface,
	#superclass : #AthensSurface,
	#instVars : [
		'form',
		'mask',
		'maskEnabled',
		'pathTransform',
		'paintTransform',
		'imageTransform',
		'fillTransform',
		'strokeTransform',
		'engine'
	],
	#category : #'Athens-Balloon-Surface'
}

{ #category : #'instance creation' }
AthensBalloonSurface class >> extent: aPoint [
	"Create a new surface with given extent."

	^ self new form: (Form extent: aPoint depth: 32)
]

{ #category : #converting }
AthensBalloonSurface >> asAthensPaintOn: aCanvas [

	^ AthensBalloonImagePaint new
		  form: self asForm;
		  yourself
]

{ #category : #converting }
AthensBalloonSurface >> asForm [

	^ form
]

{ #category : #public }
AthensBalloonSurface >> clear [

	^ self clearForm
]

{ #category : #testing }
AthensBalloonSurface >> clearForm [

	form bits: ( Bitmap new: form bits size)
]

{ #category : #clipping }
AthensBalloonSurface >> clipBy: aRectangle during: aBlock [

	| oldRect transformedClip|
	transformedClip := (pathTransform transform:aRectangle origin) corner: (pathTransform transform:aRectangle corner).
	oldRect := engine clipRect.
	engine clipRect: (oldRect intersect: transformedClip).

	aBlock ensure: [ engine clipRect: oldRect ]
]

{ #category : #accessing }
AthensBalloonSurface >> clipRect [

	^ engine clipRect
]

{ #category : #paints }
AthensBalloonSurface >> createCircularGradient: colorRamp center: aCenter radius: aRadius [

	^ AthensBalloonRadialGradient new
		colorRamp: colorRamp;
		center: aCenter;
		radius: aRadius;
		yourself
]

{ #category : #paints }
AthensBalloonSurface >> createFormPaint: aForm [

	^ AthensBalloonImagePaint new form: aForm
]

{ #category : #paints }
AthensBalloonSurface >> createLinearGradient: colorRamp start: pt1 stop: pt2 [

	^ AthensBalloonLinearGradient new
		colorRamp: colorRamp;
		origin: pt1;
		corner: pt2;
		yourself
]

{ #category : #paths }
AthensBalloonSurface >> createPath: aPathCreatingBlock [
	"Create a path from provided path builder instance"

	| builder |
	builder := AthensSimplePathBuilder new.
	aPathCreatingBlock value: builder.

	^ AthensBalloonPath new convertFromBuilder: builder
]

{ #category : #paints }
AthensBalloonSurface >> createRadialGradient: colorRamp center: aCenter radius: aRadius focalPoint: fp [

	^ self createCircularGradient: colorRamp center: aCenter radius: aRadius
]

{ #category : #paints }
AthensBalloonSurface >> createSolidColorPaint: aColor [

	^ AthensBalloonSolidColorPaint new color: aColor
]

{ #category : #paints }
AthensBalloonSurface >> createStrokePaintFor: aPaint [

	^ AthensBalloonStrokePaint new fillPaint: aPaint
]

{ #category : #masking }
AthensBalloonSurface >> disableMask [

	maskEnabled := false
]

{ #category : #drawing }
AthensBalloonSurface >> drawImage: aForm [

	engine drawImage: aForm transform: imageTransform
]

{ #category : #masking }
AthensBalloonSurface >> enableMask [

	maskEnabled := true
]

{ #category : #accessing }
AthensBalloonSurface >> engine [

	^ engine
]

{ #category : #drawing }
AthensBalloonSurface >> fillBezierShape: contours color: aColor [

	engine
		prepareForRendering;
		registerColorFill: aColor;
		fillBezierShape: contours
]

{ #category : #drawing }
AthensBalloonSurface >> fillBezierShape: anArray color: aColor width: aSmallInteger [

	self shouldBeImplemented
]

{ #category : #'rendering dispatch' }
AthensBalloonSurface >> fillPath: aPath withSolidColor: aColor [

	self fillBezierShape: aPath contoursForFilling color: aColor
]

{ #category : #drawing }
AthensBalloonSurface >> fillRectangle: rect color: aColor [

	engine
		prepareForRendering;
		registerColorFill: aColor;
		fillRectangle: rect
]

{ #category : #'rendering dispatch' }
AthensBalloonSurface >> fillRectangle: aRectangle withSolidColor: aColor [

	self fillRectangle: aRectangle color: aColor
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> fillTransform [

	^ fillTransform
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> fillTransform: anObject [

	fillTransform := anObject
]

{ #category : #accessing }
AthensBalloonSurface >> form [

	^ form
]

{ #category : #accessing }
AthensBalloonSurface >> form: aForm [

	form := aForm.
	mask := Form extent: form extent depth: 1.

	engine := AthensBalloonEngine new.
	engine
		bitBlt: (GrafPort toForm: form);
		clipRect: (0 @ 0 extent: form extent);
		edgeTransform: pathTransform;
		destOffset: 0 @ 0;
		reset
]

{ #category : #fonts }
AthensBalloonSurface >> getFreetypeFontRendererFor: aFreeTypeFont [

	^ FT2GlyphRenderer forFont: aFreeTypeFont surface: self
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> imageTransform [

	^ imageTransform
]

{ #category : #initialization }
AthensBalloonSurface >> initialize [

	super initialize.
	pathTransform := AthensAffineTransform new.
	paintTransform := AthensAffineTransform new.

	imageTransform := AthensAffineTransform new " AthensGenericTransform new".
	fillTransform := AthensAffineTransform new.
	strokeTransform := AthensAffineTransform new.
	maskEnabled := false
]

{ #category : #masking }
AthensBalloonSurface >> maskEnabled [

	^ maskEnabled
]

{ #category : #accessing }
AthensBalloonSurface >> paintTransform [

	^ paintTransform
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> pathTransform [

	^ pathTransform
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> pathTransform: anObject [

	pathTransform := anObject copy.
	engine edgeTransform: pathTransform
]

{ #category : #drawing }
AthensBalloonSurface >> strokeBezierShape: contours color: aColor width: aWidth [

	engine
		prepareForRendering;
		registerColorFill: aColor;
		strokeBezierShape: contours width: aWidth
]

{ #category : #drawing }
AthensBalloonSurface >> strokeRectangle: rect color: aColor width: aWidth [

	engine
		prepareForRendering;
		registerColorFill: aColor;
		strokeRectangle: rect width: aWidth
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> strokeTransform [

	^ strokeTransform
]

{ #category : #'accessing - matrices' }
AthensBalloonSurface >> strokeTransform: anObject [

	strokeTransform := anObject
]
