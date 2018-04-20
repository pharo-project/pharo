"
I am the FT_Outline mapping.
"
Class {
	#name : #FTOutline,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_CONTOURS',
		'OFFSET_FLAGS',
		'OFFSET_N_CONTOURS',
		'OFFSET_N_POINTS',
		'OFFSET_POINTS',
		'OFFSET_TAGS'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTOutline class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
	 short       n_contours;
    short       n_points;

    FT_Vector*  points;
    char*       tags;
    short*      contours;

    int         flags;
)
]

{ #category : #'accessing structure variables' }
FTOutline >> contours [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_CONTOURS) type: ExternalType short asPointerType
]

{ #category : #'accessing structure variables' }
FTOutline >> contours: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CONTOURS put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTOutline >> flags [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_FLAGS
]

{ #category : #'accessing structure variables' }
FTOutline >> flags: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_FLAGS put: anObject
]

{ #category : #'accessing structure variables' }
FTOutline >> n_contours [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_N_CONTOURS
]

{ #category : #'accessing structure variables' }
FTOutline >> n_contours: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_N_CONTOURS put: anObject
]

{ #category : #'accessing structure variables' }
FTOutline >> n_points [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_N_POINTS
]

{ #category : #'accessing structure variables' }
FTOutline >> n_points: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_N_POINTS put: anObject
]

{ #category : #'accessing structure variables' }
FTOutline >> points [
	"This method was automatically generated"
	^FTVector fromHandle: (handle pointerAt: OFFSET_POINTS)
]

{ #category : #'accessing structure variables' }
FTOutline >> points: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_POINTS put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTOutline >> tags [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_TAGS) type: ExternalType char asPointerType
]

{ #category : #'accessing structure variables' }
FTOutline >> tags: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_TAGS put: anObject getHandle.
]
