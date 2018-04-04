"
I map the FT_SizeMetrics type.
"
Class {
	#name : #FTSizeMetrics,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_ASCENDER',
		'OFFSET_DESCENDER',
		'OFFSET_HEIGHT',
		'OFFSET_MAX_ADVANCE',
		'OFFSET_X_PPEM',
		'OFFSET_X_SCALE',
		'OFFSET_Y_PPEM',
		'OFFSET_Y_SCALE'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTSizeMetrics class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_UShort  x_ppem;      
    FT_UShort  y_ppem;      

    FT_Fixed   x_scale;     
    FT_Fixed   y_scale;     

    FT_Pos     ascender;    
    FT_Pos     descender;   
    FT_Pos     height;      
    FT_Pos     max_advance; 
)
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> ascender [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_ASCENDER
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> ascender: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_ASCENDER put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> descender [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_DESCENDER
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> descender: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_DESCENDER put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> height [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HEIGHT
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> height: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> max_advance [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_MAX_ADVANCE
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> max_advance: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_MAX_ADVANCE put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> x_ppem [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_X_PPEM
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> x_ppem: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_X_PPEM put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> x_scale [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X_SCALE
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> x_scale: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X_SCALE put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> y_ppem [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_Y_PPEM
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> y_ppem: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_Y_PPEM put: anObject
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> y_scale [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y_SCALE
]

{ #category : #'accessing structure variables' }
FTSizeMetrics >> y_scale: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y_SCALE put: anObject
]
