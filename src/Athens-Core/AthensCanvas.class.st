"
`AthensCanvas` instance is a central object which is used to perform drawings on a surface.
Please note, that Athens does not allow you to instantiate canvas directly, instead you obtain a ready to use instance as an argument in `#drawDuring:` message, sent to athens surface:

```
surface drawDuring: [:canvas | .... ]
```

Using canvas outside a `#drawDuring:` method is highly discouraged. Doing so may lead to unpredicted behavior/data corruption/image crash.
"
Class {
	#name : #AthensCanvas,
	#superclass : #Object,
	#instVars : [
		'paint',
		'surface',
		'shape',
		'fontRenderer',
		'paintMode',
		'paintCache'
	],
	#category : #'Athens-Core-Base'
}

{ #category : #'instance creation' }
AthensCanvas class >> new [

	self error: 'An AthensCanvas are created with on:'
]

{ #category : #private }
AthensCanvas class >> on: aSurface [
	^ self basicNew initializeWithSurface: aSurface
]

{ #category : #caching }
AthensCanvas >> cacheAt: anObject ifAbsentPut: aBlock [

	^ surface cacheAt: anObject ifAbsentPut: aBlock
]

{ #category : #clipping }
AthensCanvas >> clipBy: aRectangle during: aBlock [
	"Set a clipping rectangle during drawing operations performed in aBlock.
	Note that the clipping rectangle is intersected with the currently active clipping rectangle"

	 ^surface clipBy: aRectangle during: aBlock
]

{ #category : #paths }
AthensCanvas >> createPath: aPathCreatingBlock [
	"Create new path. A path creating block should be a monadic block,
	which will get a path builder instance as argument. See AthensPathBuilder
	for available protocol.
	The resulting path object can be later used for drawing.
	"
	^ surface createPath: aPathCreatingBlock
]

{ #category : #paths }
AthensCanvas >> createRectanglePath: aRectangle [
	^ surface createRectanglePath: aRectangle
]

{ #category : #masking }
AthensCanvas >> disableMask [
	^ surface disableMask
]

{ #category : #drawing }
AthensCanvas >> draw [
	"Fill the currently selected shape with currently selected paint"

	^ shape paintFillsUsing: paint on: self
]

{ #category : #drawing }
AthensCanvas >> draw: anObject [
	"A generic dispatch method to draw anObject on receiver.
	the object should understand #drawOnAthensCanvas: message"
	^ anObject drawOnAthensCanvas: self
]

{ #category : #drawing }
AthensCanvas >> drawShape: anObject [
	"A convenience method, which sets the current shape to anObject and then fills it
	with currently selected paint"
	self
		setShape: anObject;
		draw
]

{ #category : #'drawing text' }
AthensCanvas >> drawString: aString [
	"Draw a string using currently selected font.
	Answer a total advance of rendered string "
	^ fontRenderer renderCharacters: aString from: 1 to: aString size
]

{ #category : #'drawing text' }
AthensCanvas >> drawString: aString from: start to: end [
	"Draw a portion of string using currently selected font.
	Answer a total advance of rendered portion"
	^ fontRenderer renderCharacters: aString from: start to: end
]

{ #category : #masking }
AthensCanvas >> enableMask [
	^ surface enableMask
]

{ #category : #caching }
AthensCanvas >> flushCacheAt: anObject [
	"Flush (delete) any cached value(s) identified by given object, anObject.
	A surface using identity comparison for object identifiers.

	Answer receiver.
	"

	surface flushCacheAt: anObject
]

{ #category : #initialization }
AthensCanvas >> initializeWithSurface: anAthensSurface [
	surface := anAthensSurface
]

{ #category : #clipping }
AthensCanvas >> isVisible: aRectangle [
	"Check if rectangle are not fully clipped by current clipping coordinates"

	^ true
]

{ #category : #accessing }
AthensCanvas >> paint [
	^ paint
]

{ #category : #paint }
AthensCanvas >> paintCache [

	^ paintCache ifNil: [
		paintCache := LRUCache new maximumWeight: 100 ]
]

{ #category : #accessing }
AthensCanvas >> paintMode [
	"Answer the current paint mode controller.
	The answered object should conform to AthensPaintMode public protocol."

	^ paintMode
]

{ #category : #'transformation matrices' }
AthensCanvas >> paintTransform [
	"Answer the paint transformation matrix controller.
	The answered object should conform to AthensTransform public protocol."

	^ surface paintTransform
]

{ #category : #'transformation matrices' }
AthensCanvas >> pathTransform [
	"Answer the path transformation matrix controller.
	The answered object should conform to AthensTransform public protocol."
	^ surface pathTransform
]

{ #category : #'drawing text' }
AthensCanvas >> setFont: aFont [
	"Set the current font of receiver. Font object should answer a glyph renderer instance,
	compatible with glyph renderer protocol"
	^ fontRenderer := aFont glyphRendererOn: surface
]

{ #category : #paint }
AthensCanvas >> setPaint: aPaint [
	"set the current paint of receiver"
	paint = aPaint ifTrue:[ ^ paint ].
	paint := self paintCache at: aPaint ifAbsentPut: [ aPaint asAthensPaintOn: self ].
	^ paint
]

{ #category : #accessing }
AthensCanvas >> setShape: anObject [
	"Set the current shape of receiver"
	shape := anObject asAthensShapeOn: self
]

{ #category : #paint }
AthensCanvas >> setStrokePaint: aPaint [
	"use paint as stroke paint, note conversion to #asStrokePaintOn:"
	paint := (aPaint asAthensPaintOn: self) asStrokePaintOn: self.
	^ paint
]

{ #category : #accessing }
AthensCanvas >> surface [
	"Answer an Athens surface, to which receiver is bound to"
	^ surface
]
