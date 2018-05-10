"
I represent the FT_BBox type
"
Class {
	#name : #FTBBox,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_XMAX',
		'OFFSET_XMIN',
		'OFFSET_YMAX',
		'OFFSET_YMIN'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTBBox class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
	 FT_Pos  xMin;
	 FT_Pos  yMin;
    FT_Pos  xMax; 
	 FT_Pos  yMax;
)
]

{ #category : #'accessing structure variables' }
FTBBox >> xMax [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XMAX
]

{ #category : #'accessing structure variables' }
FTBBox >> xMax: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XMAX put: anObject
]

{ #category : #'accessing structure variables' }
FTBBox >> xMin [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XMIN
]

{ #category : #'accessing structure variables' }
FTBBox >> xMin: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XMIN put: anObject
]

{ #category : #'accessing structure variables' }
FTBBox >> yMax [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YMAX
]

{ #category : #'accessing structure variables' }
FTBBox >> yMax: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YMAX put: anObject
]

{ #category : #'accessing structure variables' }
FTBBox >> yMin [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YMIN
]

{ #category : #'accessing structure variables' }
FTBBox >> yMin: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YMIN put: anObject
]
