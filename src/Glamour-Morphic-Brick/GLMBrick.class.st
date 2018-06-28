"
A GLMBrick is the superclass of all Bricks. It subclasses cleaned morph with deleted layouting mechanism

Instance Variables
	brickBounds:		<GLMBrickBounds>
	ports:		<Dictionary>

brickBounds
	- wrapper object for brick geometry.  
		holds margin, padding properties.,
		responsible for calculation of outer/inner bounds depending on margin/padding

ports
	- dictionary of phlow bindings. is not implemented yet

"
Class {
	#name : #GLMBrick,
	#superclass : #GLMBrickedMorph,
	#traits : 'GLMBrickPropertiesTrait + GLMBrickLayoutTrait + GLMBrickStructureTrait + (GLMBrickGeometryTrait - {#privateBounds:})',
	#classTraits : 'GLMBrickPropertiesTrait classTrait + GLMBrickLayoutTrait classTrait + GLMBrickStructureTrait classTrait + GLMBrickGeometryTrait classTrait',
	#instVars : [
		'brickBounds',
		'brickApi'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Core'
}

{ #category : #examples }
GLMBrick class >> exampleZindexOpen [
	"
	self exampleZindexOpen
	"
	self gtExampleZindex openInBrickWindowLabeled: 'Z-Index example'
]

{ #category : #examples }
GLMBrick class >> gtExampleZindex [
	<gtExample>
	<label: 'Example of z-index support in Brick'>
	
	^ GLMBrick new
		hSpaceFill;
		vSpaceFill;
		color: Color blue;
		addBrickBack: (
			GLMBrick new
				color: Color green;
				brickBounds: (100@100 extent: 100@100);
				zIndex: 3);
		addBrickBack: (
			GLMBrick new
				color: Color yellow;
				brickBounds: (50@50 extent: 200@100);
				zIndex: 2);
		addBrickBack: (
			GLMBrick new
				color: Color red;
				brickBounds: (0@0 extent: 300@200);
				zIndex: 1);
		yourself
]

{ #category : #utilities }
GLMBrick class >> increaseFormExtentCentered: aForm to: aPoint [
	| icon rect destination |
	
	aForm ifNil: [ ^ nil ].
	rect := Rectangle origin: 0@0 extent: aPoint.
	destination := (rect extent - aForm extent) / 2.
	icon := Form extent: rect extent depth: aForm depth.
	icon copyBits: rect from: aForm at: destination clippingBox: icon boundingBox rule: Form over fillColor: nil.
	^ icon
]

{ #category : #utilities }
GLMBrick class >> makeForm32Bit: aForm [
	aForm ifNil: [ ^ nil ].
	aForm depth = 32 ifTrue: [ ^ aForm ].
	aForm depth: 32.
	aForm fixAlpha.
	^ aForm.
]

{ #category : #utilities }
GLMBrick class >> makeFormTransparent: aForm factor: aFloat [
	"a float must be in interval [0.0, 1.0]"
	| a |
	
	aForm ifNil: [ ^ nil ].
	aForm depth = 32 ifFalse: [^ aForm ].
	
	a := (aFloat * 255) asInteger.
	1 to: aForm bits size do: [:i |
		| v r g b |
		v := aForm bits at: i.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		aForm bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b ].
	^ aForm 
]

{ #category : #utilities }
GLMBrick class >> resizeImage: anImage scale: aScale [

	((aScale > (1@0)) | (aScale > (0@1))) ifTrue: [
		^ self increaseFormExtentCentered: anImage to: (anImage extent * aScale).
	].

	aScale = (1@1) ifTrue: [
		^ anImage
	].
	
	^ anImage magnify: anImage boundingBox by: aScale smoothing: 2.
]

{ #category : #utilities }
GLMBrick class >> scaleForm: aForm by: aPoint [

	aForm ifNil: [ ^ nil ].
	^ aForm magnify: aForm boundingBox by: aPoint smoothing: 2
]

{ #category : #utilities }
GLMBrick class >> scaleForm: aForm to: aPoint [
	|scaleX scaleY|
	
	aForm ifNil: [ ^ nil ].
	scaleX := aPoint x / aForm width.
	scaleY := aPoint y / aForm height.
	^ self scaleForm: aForm by: scaleX @ scaleY.
]

{ #category : #utilities }
GLMBrick class >> scalePatchForm: aForm size: aPoint [
	"aPoint - width and height of new form
	
|form|

form := PatchForm scalePatchForm: (ImageReadWriter formFromFileNamed: 'abc_calculator_button_normal.p.png') size:800@800.
(ImageMorph withForm: form) openInWindow.

form := ImageReadWriter formFromFileNamed: 'abc_calculator_button_normal.png'.
form := PatchForm scaleForm: form by: (800/(form extent x))@(800/(form extent y)).
(ImageMorph withForm: form) openInWindow.
	
	"
	
	|x0 x1 y0 y1 width height newForm tmpForm color bottomHeight rightWidth newMiddleWidth newMiddleHeight|
	
	aForm ifNil: [ ^ nil ].
	
	color := Color red.
	x0 := x1 := y0 := y1 := 0.
	width := aForm extent x.
	height := aForm extent y.
	
	(0 to: width-1) collect: [ :each |
		(aForm colorAt: each@0) = color ifTrue: [ 
			x0 = 0 ifTrue: [ 
				x0 := each.
			]
		] ifFalse:[
			(x0 > 0 & x1 = 0) ifTrue:[
				x1 := each.
			].
		].
	].

	(0 to: height) collect: [ :each |
		(aForm colorAt: 0@each) = color ifTrue: [ 
			y0 = 0 ifTrue: [ 
				y0 := each.
			]
		] ifFalse:[
			(y0 > 0 & y1 = 0) ifTrue:[
				y1 := each.
			].
		].
	].

	rightWidth := width - x1 -1.
	bottomHeight := height - y1 - 1.
	newMiddleWidth := aPoint x - x0 - rightWidth + 1.
	newMiddleHeight := aPoint y - y0 - bottomHeight + 1.
	
	newForm := Form extent: aPoint depth: aForm depth.
	"left top corner"
	newForm copyBits: (1@1 corner: x0@y0)
		from: aForm at: 0@0 clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	"left bottom corner"
	newForm copyBits: (1@y1 corner: x0@(height - 1))
		from: aForm at: 0@(aPoint y - bottomHeight) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	"right top corner"
	newForm copyBits: (x1@1 corner: (width-1)@y0)
		from: aForm at: (aPoint x - rightWidth)@0 clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	"right bottom corner"
	newForm copyBits: (x1@y1 corner: (width - 1)@(height - 1))
		from: aForm at: (aPoint x - rightWidth)@(aPoint y - bottomHeight) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	
	"top middle block"
	tmpForm := aForm copy: (x0@1 corner: x1@y0).
	tmpForm := self scaleForm: tmpForm by: ((newMiddleWidth)/(x1-x0))@1.
	newForm copyBits: (0@0 corner: (tmpForm extent x)@(tmpForm extent y))
		from: tmpForm at: (x0-1)@0 clippingBox: newForm boundingBox rule: Form over fillColor: nil.
		
	"bottom middle block"
	tmpForm := aForm copy: (x0@y1 corner: x1@height).
	tmpForm := self scaleForm: tmpForm by: ((newMiddleWidth)/(x1-x0))@1.
	newForm copyBits: (0@0 corner: (tmpForm extent x)@(tmpForm extent y))
		from: tmpForm at: (x0-1)@(aPoint y - bottomHeight) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	
	"left middle block"
	tmpForm := aForm copy: (1@y0 corner: x0@y1).
	tmpForm := self scaleForm: tmpForm by: 1@((newMiddleHeight)/(y1-y0)).
	newForm copyBits: (0@0 corner: (tmpForm extent x)@(tmpForm extent y))
		from: tmpForm at: 0@(y0-1) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	
	"right middle block"
	tmpForm := aForm copy: (x1@y0 corner: width@y1).
	tmpForm := self scaleForm: tmpForm by: 1@((newMiddleHeight)/(y1-y0)).
	newForm copyBits: (0@0 corner: (tmpForm extent x)@(tmpForm extent y))
		from: tmpForm at: (aPoint x - rightWidth)@(y0-1) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
		
	"middle block"
	tmpForm := aForm copy: (x0@y0 corner: x1@y1).
	tmpForm := self scaleForm: tmpForm by: ((newMiddleWidth)/(x1-x0))@((newMiddleHeight)/(y1-y0)).
	newForm copyBits: (0@0 corner: (tmpForm extent x)@(tmpForm extent y))
		from: tmpForm at: (x0-1)@(y0-1) clippingBox: newForm boundingBox rule: Form over fillColor: nil.
	
	^ newForm.
	

]

{ #category : #'brick-properties' }
GLMBrick >> announcer [

	^ super announcer
]

{ #category : #'brick-structure-converting' }
GLMBrick >> asMorph [

	^ self asBrickedMorph
]

{ #category : #'brick-geometry' }
GLMBrick >> bounds [

	^ self globalBounds
]

{ #category : #'morph-common' }
GLMBrick >> boundsInWorld [

	^ self globalBounds
]

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrick >> brickApi [

	^ brickApi ifNil: [ brickApi := GLMBrickWrapper on: self ]
]

{ #category : #'morph-drawing' }
GLMBrick >> changed [
	"Report that the area occupied by this morph should be redrawn.
	Fixed to include submorphs outside the outerBounds."
	
	"fixed direct reference to fullBounds, causing wrong results, because fullBounds are not being updated when parent's or child's bounds changed"
	self world
		ifNil: [ ^ self ].
	
	self invalidRect: self globalClippingBounds
]

{ #category : #'morph-drawing' }
GLMBrick >> changedWith: aBlock [

	self changed.
	aBlock cull: self.
	self changed.
]

{ #category : #'brick-properties' }
GLMBrick >> currentColor [
	^ self color
]

{ #category : #initialization }
GLMBrick >> defaultRenderer [
	^ GLMBrickRenderer new
]

{ #category : #'morph-drawing' }
GLMBrick >> drawOn: aCanvas [
	"super drawOn: aCanvas."
	aCanvas
		translateBy: self globalBounds origin - self brickBounds origin
		during: [ :canvas | self onDraw: canvas ]
]

{ #category : #'morph-drawing' }
GLMBrick >> drawOnAthensCanvas: anAthensCanvas [

	anAthensCanvas pathTransform restoreAfter: [
					anAthensCanvas pathTransform translateBy: (self globalBounds origin - self brickBounds origin).
					self onDrawAthensCanvas: anAthensCanvas ]
]

{ #category : #'morph-drawing' }
GLMBrick >> drawSubmorphsOn: aCanvas [
	| area zIndexedBricks |
	
	self submorphs isEmpty ifTrue: [ ^ self ].

	"make sure my children will not go away of my inner bounds (we take care about paddings)"
	area := aCanvas clipRect intersect: self globalInnerBounds ifNone: (0 @ 0 extent: 0 @ 0).
	"do smart drawing logic here"
	area width = 0 | (area height = 0) ifTrue: [ 
			"draw nothing"
			^ self ].

	zIndexedBricks := OrderedCollection new.
	
	self subbricks
		do: [ :each | 
			| intersection canvas globalBounds |
			each ifNotNil: [
			globalBounds := each globalClippingBounds.

			(area intersects: globalBounds) ifTrue: [
				| zIndex |
				zIndex := each layoutProperties zIndex brickValue: each.
				zIndex = 1 ifTrue: [
					"immediately draw ones with the lowest possible z-index.
					With this approach performance doesn't degrade"
					intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
					canvas := aCanvas copyClipRect: intersection.
					each fullDrawOn: canvas ]
					"if z-index is not 1 we add it to collection, in order to sort and draw as second attempt"
				ifFalse: [ zIndexedBricks add: each ] ] ] ].
		
		zIndexedBricks sort: [ :first :second | (first layoutProperties zIndex brickValue: first) < (second layoutProperties zIndex brickValue: second) ].
		
		zIndexedBricks do: [ :each |
			| intersection canvas globalBounds |
			each ifNotNil: [
			globalBounds := each globalClippingBounds.

			intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
			canvas := aCanvas copyClipRect: intersection.
			each fullDrawOn: canvas ] ]
		
]

{ #category : #'morph-drawing' }
GLMBrick >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	| area zIndexedBricks |
	
	self submorphs isEmpty ifTrue: [ ^ self ].

	"make sure my children will not go away of my inner bounds (we take care about paddings)"
	area := anAthensCanvas clipRect intersect: self globalInnerBounds ifNone: (0 @ 0 extent: 0 @ 0).
	"do smart drawing logic here"
	area width = 0 | (area height = 0) ifTrue: [ 
			"draw nothing"
			^ self ].
	
	zIndexedBricks := OrderedCollection new.
	
	self subbricks
		do: [ :each | 
			| intersection  globalBounds |
			each ifNotNil: [ self flag: 'fix nil case'.
			globalBounds := each isBrick
				ifTrue: [ each globalBounds ]
				ifFalse: [ each fullBounds ].
				
			(area intersects: globalBounds) ifTrue: [
				| zIndex |
				zIndex := each layoutProperties zIndex brickValue: each.
				zIndex = 1 ifTrue: [
					"immediately draw ones with the lowest possible z-index.
					With this approach performance doesn't degrade"
					intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
					anAthensCanvas clipBy: intersection during: [ each fullDrawOnAthensCanvas: anAthensCanvas ] ]
					"if z-index is not 1 we add it to collection, in order to sort and draw as second attempt"
				ifFalse: [ zIndexedBricks add: each ] ] ] ].
		
		"z-index drawing section"
		zIndexedBricks sort: [ :first :second | (first layoutProperties zIndex brickValue: first) < (second layoutProperties zIndex brickValue: second) ].
		
		zIndexedBricks do: [ :each |
			| intersection globalBounds |
			each ifNotNil: [ self flag: 'fix nil case'.
			globalBounds := each isBrick
				ifTrue: [ each globalBounds ]
				ifFalse: [ each fullBounds ].
				
			intersection := area intersect: globalBounds ifNone: (0 @ 0 extent: 0 @ 0).
			anAthensCanvas clipBy: intersection during: [ each fullDrawOnAthensCanvas: anAthensCanvas ] ] ]
		
]

{ #category : #'brick-geometry' }
GLMBrick >> drawingBounds [
	^ self renderer drawingBounds: self
]

{ #category : #'morph-common' }
GLMBrick >> fullBounds [

	^ self globalBounds
]

{ #category : #'morph-drawing' }
GLMBrick >> fullDrawOn: aCanvas [
	"fixes bug in canvas, when it tries to draw a morph even if clip rect has size 0"
	
	aCanvas clipRect hasPositiveExtent
		ifFalse: [ ^ self ].	
	
	super fullDrawOn: aCanvas
]

{ #category : #'brick-geometry' }
GLMBrick >> globalClippingBounds [
	^ self owner
		ifNotNil: [ :brick | self drawingBounds translateBy: brick globalBoundsTranslateFactor ]
		ifNil: [ self drawingBounds ]
]

{ #category : #'morph-common' }
GLMBrick >> haloClass [
	"Answer the name of the desired kind of HaloMorph to launch on behalf of the receiver"

	^ #GLMHaloBrick
]

{ #category : #'private-extension' }
GLMBrick >> hasProperty: aSymbol [ 
	
	^ super hasProperty: aSymbol
]

{ #category : #initialization }
GLMBrick >> initialize [
	super initialize.
	color := Color transparent.
	self fillStyle: nil.
	submorphs := OrderedCollection new.
	
	self
		layoutProperties: GLMBrickLayoutProperties new;
		useHorizontalLinearLayout;
		renderer: self defaultRenderer
]

{ #category : #'brick-testing' }
GLMBrick >> isFitsInParent [

	^ self owner
		"if our owner is a brick"
		ifNotNil: [ self owner globalBounds containsRect: self globalBounds ]
		"if our owner is a morph"
		ifNil: [ self owner ifNotNil: [ self owner bounds containsRect: self globalBounds ] ifNil: [ ^ true ] ]
]

{ #category : #'morph-common' }
GLMBrick >> isMorphBrick [

	^ false
]

{ #category : #'brick-layout' }
GLMBrick >> layoutPolicy [

	^ super layoutPolicy
]

{ #category : #'brick-layout' }
GLMBrick >> layoutPolicy: aLayout [
	
	super layoutPolicy: aLayout
]

{ #category : #'brick-layout' }
GLMBrick >> layoutProperties [
	
	^ super layoutProperties
]

{ #category : #'brick-layout' }
GLMBrick >> layoutProperties: aLayoutProperties [ 

	super layoutProperties: aLayoutProperties
]

{ #category : #'brick-drawing' }
GLMBrick >> onDraw: aCanvas [

	self renderer render: self on: aCanvas. 
]

{ #category : #'brick-drawing' }
GLMBrick >> onDrawAthensCanvas: aCanvas [

	self renderer render: self onAthensCanvas: aCanvas. 
]

{ #category : #'brick-properties-events' }
GLMBrick >> onThemerChanged [

	self changed
]

{ #category : #'brick-geometry' }
GLMBrick >> outerBoundsInParent [
	^ self owner
		ifNotNil: [ self wrappedBounds outerBounds translateBy: self owner brickBounds origin ]
		ifNil: [ self wrappedBounds outerBounds ]
]

{ #category : #'brick-structure' }
GLMBrick >> owner [

	^ owner
]

{ #category : #'morph-common' }
GLMBrick >> privateMoveBy: delta [ 
	"Private! Use 'position:' instead."
	bounds := bounds translateBy: delta.
	fullBounds ifNotNil: [fullBounds := fullBounds translateBy: delta].
]

{ #category : #'private-structure' }
GLMBrick >> privateOwner: aBrick [

	owner := aBrick
]

{ #category : #'private-structure' }
GLMBrick >> privateSubbricks: aCollection [

	submorphs := aCollection
]

{ #category : #'morph-common' }
GLMBrick >> removeAllMorphs [
	| oldMorphs myWorld |
	"we don't invalidate world"
	myWorld := self world.
	submorphs do: [:m | myWorld ifNotNil: [ m outOfWorld: myWorld ]. m privateOwner: nil].
	oldMorphs := submorphs.
	submorphs := EmptyArray.
	oldMorphs do: [ :m | self removedMorph: m ].

]

{ #category : #'private-extension' }
GLMBrick >> removeProperty: aSymbol [
	
	super removeProperty: aSymbol
]

{ #category : #'morph-common' }
GLMBrick >> setConstrainedPosition: aPoint hangOut: partiallyOutside [
	"Change the position of this morph and and all of its submorphs to aPoint, but don't let me go outside my owner's bounds."

	| trialRect delta boundingRectangle |
	self owner ifNil:[ ^ self ].
	trialRect := 0@0 extent: self outerBounds extent.
	boundingRectangle := (self owner paddingLeft @ self owner paddingTop) extent: self owner innerBounds extent.
	delta := (((aPoint + trialRect corner) min: (boundingRectangle corner)) - trialRect corner) max: boundingRectangle origin.
	
	self position: delta.
	self changed

]

{ #category : #'private-extension' }
GLMBrick >> setProperty: aSymbol toValue: anObject [
	
	super setProperty: aSymbol toValue: anObject
]

{ #category : #'brick-structure' }
GLMBrick >> subbricks [

	^ self submorphs
]

{ #category : #'morph-common' }
GLMBrick >> submorphAfter [

	^ self subbrickAfter
]

{ #category : #'morph-common' }
GLMBrick >> submorphBefore [

	^ self subbrickBefore
]

{ #category : #'morph-common' }
GLMBrick >> taskThumbnailOfSize: thumbExtent [
	"Answer a new task thumbnail for the receiver."

	|f t r|
	r := self globalBounds scaledAndCenteredIn: (0@0 extent: thumbExtent).
	f := Form extent: r extent depth: Display depth.
	t := MatrixTransform2x3 withScale: f extent / self extent.
	f getCanvas
		transformBy: t
		clippingTo: f boundingBox
		during: [:c | c translateBy: self globalBounds topLeft negated during: [:ct | self fullDrawOn: ct]]
		smoothing: 2.
	^ImageMorph new
		form: f
]

{ #category : #'morph-common' }
GLMBrick >> updateMorphBounds [

	self withAllSubbricks select: #isMorphBrick thenDo: #onLayouted
]

{ #category : #'private-extension' }
GLMBrick >> valueOfProperty: aSymbol [
	
	^ super valueOfProperty: aSymbol
]

{ #category : #'private-extension' }
GLMBrick >> valueOfProperty: aSymbol ifAbsent: aBlock [
	
	^ super valueOfProperty: aSymbol ifAbsent: aBlock
]

{ #category : #'private-extension' }
GLMBrick >> valueOfProperty: aSymbol ifAbsentPut: aBlock [ 
	
	^ super valueOfProperty: aSymbol ifAbsentPut: aBlock
]

{ #category : #'brick-geometry' }
GLMBrick >> wrappedBounds [

	^ brickBounds ifNil: [ brickBounds := GLMBrickBounds new ]
]

{ #category : #'brick-geometry' }
GLMBrick >> wrappedBounds: aBounds [

	brickBounds := aBounds
]
