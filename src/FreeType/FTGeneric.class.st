"
I implement the FT_Generic type
"
Class {
	#name : #FTGeneric,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_DATA',
		'OFFSET_FINALIZER'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTGeneric class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
	 void*  data;
    void*  finalizer;)
]

{ #category : #'accessing structure variables' }
FTGeneric >> data [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_DATA) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGeneric >> data: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_DATA put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGeneric >> finalizer [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_FINALIZER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGeneric >> finalizer: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FINALIZER put: anObject getHandle.
]
