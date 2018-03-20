"
I represent the FT_Bitmap_Size type.
"
Class {
	#name : #FTBitmapSize,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_HEIGHT',
		'OFFSET_SIZE',
		'OFFSET_WIDTH',
		'OFFSET_X_PPEM',
		'OFFSET_Y_PPEM'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTBitmapSize class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_Short  height;
    FT_Short  width;

    FT_Pos    size;

    FT_Pos    x_ppem;
    FT_Pos    y_ppem;	
)
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> height [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_HEIGHT
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> height: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> size [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_SIZE
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> size: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_SIZE put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> width [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_WIDTH
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> width: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_WIDTH put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> x_ppem [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X_PPEM
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> x_ppem: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X_PPEM put: anObject
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> y_ppem [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y_PPEM
]

{ #category : #'accessing structure variables' }
FTBitmapSize >> y_ppem: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y_PPEM put: anObject
]
