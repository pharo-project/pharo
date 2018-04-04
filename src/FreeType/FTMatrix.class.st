"
I implement the FT_Matrix type, I am a matrix of 2x2 for transformations.
"
Class {
	#name : #FTMatrix,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_XX',
		'OFFSET_XY',
		'OFFSET_YX',
		'OFFSET_YY'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTMatrix class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(    
		FT_Fixed  	xx
		FT_Fixed 	xy
    	FT_Fixed  	yx 
		FT_Fixed 	yy
)
]

{ #category : #'accessing structure variables' }
FTMatrix >> xx [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XX
]

{ #category : #'accessing structure variables' }
FTMatrix >> xx: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XX put: anObject
]

{ #category : #'accessing structure variables' }
FTMatrix >> xy [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XY
]

{ #category : #'accessing structure variables' }
FTMatrix >> xy: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_XY put: anObject
]

{ #category : #'accessing structure variables' }
FTMatrix >> yx [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YX
]

{ #category : #'accessing structure variables' }
FTMatrix >> yx: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YX put: anObject
]

{ #category : #'accessing structure variables' }
FTMatrix >> yy [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YY
]

{ #category : #'accessing structure variables' }
FTMatrix >> yy: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_YY put: anObject
]
