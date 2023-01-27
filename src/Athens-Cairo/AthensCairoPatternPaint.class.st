"
i am abstract class for different kinds of paints in cairo backend.
"
Class {
	#name : #AthensCairoPatternPaint,
	#superclass : #FFIExternalObject,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #private }
AthensCairoPatternPaint class >> finalizeResourceData: handle [

	self primDestroyPattern: handle value
]

{ #category : #private }
AthensCairoPatternPaint class >> primDestroyPattern: aHandle [
	^ self ffiCall: #(void cairo_pattern_destroy (ulong aHandle) )
]

{ #category : #converting }
AthensCairoPatternPaint >> asAthensPaintOn: anAthensCanvas [
	^ self
]

{ #category : #converting }
AthensCairoPatternPaint >> asStrokePaintOn: aCanvas [
	^ aCanvas surface createStrokePaintFor: self
]

{ #category : #drawing }
AthensCairoPatternPaint >> athensFillPath: path on: aCanvas [

	aCanvas
		newPath;
		loadPath: path.
	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.

	aCanvas fill
]

{ #category : #drawing }
AthensCairoPatternPaint >> athensFillRectangle: aRectangle on: aCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	aCanvas
		rectangleX: aRectangle left
		y: aRectangle top
		width: aRectangle width
		height: aRectangle height.

	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.
	aCanvas fill
]

{ #category : #drawing }
AthensCairoPatternPaint >> athensLoadOn: anAthensCanvas [
	"
	Note: The pattern's transformation matrix will be locked to the user space in effect at the time of cairo_set_source(). This means that further modifications of the current transformation matrix will not affect the source pattern. See cairo_pattern_set_matrix().
	"

	self isSuccess
		ifTrue:[ anAthensCanvas primSetSource: self ]
		ifFalse:[ anAthensCanvas setSourceR: 1.0 g: 0.0 b: 0.0 a: 1.0]
]

{ #category : #initialization }
AthensCairoPatternPaint >> defaultExtend [
"The default extend mode is CAIRO_EXTEND_NONE for surface patterns and CAIRO_EXTEND_PAD for gradient patterns.
"

	^  CAIRO_EXTEND_NONE
]

{ #category : #initialization }
AthensCairoPatternPaint >> initialize [
	handle value = 0
		ifTrue: [ self error: 'Error creating new paint' ].
	"register to be finalized"
	FFIExternalResourceManager addResource: self data: handle
]

{ #category : #testing }
AthensCairoPatternPaint >> isPatternTypeMismatch [
	^ self status = CAIRO_STATUS_PATTERN_TYPE_MISMATCH
]

{ #category : #testing }
AthensCairoPatternPaint >> isSuccess [
	^ self status  =	CAIRO_STATUS_SUCCESS
]

{ #category : #drawing }
AthensCairoPatternPaint >> loadOnCairoCanvas: aCanvas [
	"
	Note: The pattern's transformation matrix will be locked to the user space in effect at the time of cairo_set_source(). This means that further modifications of the current transformation matrix will not affect the source pattern. See cairo_pattern_set_matrix().
	"

	self isSuccess
		ifTrue:[ aCanvas primSetSource: self ]
		ifFalse:[ aCanvas setSourceR: 1.0 g: 0.0 b: 0.0 a: 1.0]
]

{ #category : #drawing }
AthensCairoPatternPaint >> maskOn: anAthensCanvas [
"
A drawing operator that paints the current source using the alpha channel of pattern as a mask. (Opaque areas of pattern are painted with the source, transparent areas are not painted.)
"
	^ self ffiCall: #(void cairo_mask(cairo_t anAthensCanvas, self))
]

{ #category : #accessing }
AthensCairoPatternPaint >> noRepeat [
	self primSetExtend: CAIRO_EXTEND_NONE
]

{ #category : #primitives }
AthensCairoPatternPaint >> primSetExtend: extend [
	^ self ffiCall:#(
		void cairo_pattern_set_extend ( self , ulong extend) )
]

{ #category : #primitives }
AthensCairoPatternPaint >> primSetSourceOn: aCanvas [
	^ self ffiCall: #(void cairo_set_source ( cairo_t  aCanvas,  self))
]

{ #category : #accessing }
AthensCairoPatternPaint >> reflect [
	self primSetExtend: CAIRO_EXTEND_REFLECT
]

{ #category : #accessing }
AthensCairoPatternPaint >> repeat [
	self primSetExtend: CAIRO_EXTEND_REPEAT
]

{ #category : #accessing }
AthensCairoPatternPaint >> setExtend: aSymbol [
	"
	typedef enum {
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT,
    CAIRO_EXTEND_PAD
} cairo_extend_t;
	"
	| enum |
	enum := self defaultExtend.
	aSymbol = #None ifTrue: [ enum := CAIRO_EXTEND_NONE ].
	aSymbol = #Repeat ifTrue: [ enum := CAIRO_EXTEND_REPEAT ].
	aSymbol = #Reflect ifTrue: [ enum := CAIRO_EXTEND_REFLECT ].
	aSymbol = #Pad ifTrue: [ enum := CAIRO_EXTEND_PAD ].
	self primSetExtend: enum
]

{ #category : #accessing }
AthensCairoPatternPaint >> setMatrix: m [
	^ self ffiCall: #(
		void
			cairo_pattern_set_matrix ( self, AthensCairoMatrix * m)
		)
]

{ #category : #accessing }
AthensCairoPatternPaint >> status [
	^ self ffiCall: #(int cairo_pattern_status (cairo_pattern_t  self))
]
