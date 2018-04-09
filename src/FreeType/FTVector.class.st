"
I represent a 2D vector of type FT_Vector. I am used to do transformations.
"
Class {
	#name : #FTVector,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_X',
		'OFFSET_Y'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTVector class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(    
	 FT_Pos  x;
    FT_Pos  y;)
]

{ #category : #'accessing structure variables' }
FTVector >> x [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
FTVector >> x: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
FTVector >> y [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
FTVector >> y: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_Y put: anObject
]
