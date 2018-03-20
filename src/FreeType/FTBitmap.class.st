"
I represent the FT_Bitmap type
"
Class {
	#name : #FTBitmap,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_BUFFER',
		'OFFSET_NUM_GRAYS',
		'OFFSET_PALETTE',
		'OFFSET_PALETTE_MODE',
		'OFFSET_PITCH',
		'OFFSET_PIXEL_MODE',
		'OFFSET_ROWS',
		'OFFSET_WIDTH'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTBitmap class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    uint   rows;
    uint   width;
    int    pitch;
    void*  buffer;
    ushort num_grays;
    uchar  pixel_mode;
    uchar  palette_mode;
    void*  palette;	
)
]

{ #category : #'accessing structure variables' }
FTBitmap >> buffer [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_BUFFER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTBitmap >> buffer: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_BUFFER put: anObject getHandle.
]

{ #category : #initialization }
FTBitmap >> initializeFromForm: aForm pixelMode: pixelMode [
	| bitmapHeight width depth wordsPerRow numGrays pitch pixelModeToUse |
	self width: aForm width.
	bitmapHeight := aForm height.

	width := aForm width.
	depth := aForm depth.

	depth < 0
		ifTrue: [ depth := depth * -1 ].
	depth = 1
		ifTrue: [ wordsPerRow := ((width + 16r1F) / 32) floor.
			numGrays := 1 ]
		ifFalse: [ depth = 8
				ifFalse: [ self error: 'Invalid depth' ].
			wordsPerRow := ((width + 3) / 4) floor.
			numGrays := 256 ].

	"aForm depth: depth."

	pixelMode
		ifNil: [ depth = 1
				ifTrue: [ pixelModeToUse := 1	"FT_PIXEL_MODE_MONO" ]
				ifFalse: [ pixelModeToUse := 2	"FT_PIXEL_MODE_GRAY" ] ]
		ifNotNil: [ pixelModeToUse := pixelMode ].

	pitch := wordsPerRow * 4.
	self rows: bitmapHeight.
	self width: width.
	self pitch: pitch.
	self num_grays: numGrays.
	self pixel_mode: pixelModeToUse
]

{ #category : #'accessing structure variables' }
FTBitmap >> num_grays [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_NUM_GRAYS
]

{ #category : #'accessing structure variables' }
FTBitmap >> num_grays: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_NUM_GRAYS put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmap >> palette [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PALETTE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTBitmap >> palette: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PALETTE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTBitmap >> palette_mode [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PALETTE_MODE
]

{ #category : #'accessing structure variables' }
FTBitmap >> palette_mode: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PALETTE_MODE put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmap >> pitch [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PITCH
]

{ #category : #'accessing structure variables' }
FTBitmap >> pitch: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PITCH put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmap >> pixel_mode [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PIXEL_MODE
]

{ #category : #'accessing structure variables' }
FTBitmap >> pixel_mode: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PIXEL_MODE put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmap >> rows [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_ROWS
]

{ #category : #'accessing structure variables' }
FTBitmap >> rows: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_ROWS put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmap >> setBuffer: anAddress [
	handle pointerAt: OFFSET_BUFFER put: anAddress
]

{ #category : #'accessing structure variables' }
FTBitmap >> width [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WIDTH
]

{ #category : #'accessing structure variables' }
FTBitmap >> width: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WIDTH put: anObject
]
