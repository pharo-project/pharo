"
I implement the FT_CharMap_Rec
"
Class {
	#name : #FTCharMapRec,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_ENCODING',
		'OFFSET_ENCODING_ID',
		'OFFSET_FACE',
		'OFFSET_PLATFORM_ID'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTCharMapRec class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_Face      face;
    FT_Encoding  encoding;
    FT_UShort    platform_id;
    FT_UShort    encoding_id;
)
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> encoding [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_ENCODING
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> encoding: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_ENCODING put: anObject
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> encoding_id [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_ENCODING_ID
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> encoding_id: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_ENCODING_ID put: anObject
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> face [
	"This method was automatically generated"
	^FTFaceRec fromHandle: (handle pointerAt: OFFSET_FACE)
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> face: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FACE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> platform_id [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PLATFORM_ID
]

{ #category : #'accessing structure variables' }
FTCharMapRec >> platform_id: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PLATFORM_ID put: anObject
]
