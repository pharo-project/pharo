"
I implement the FT_LIST_REC, I am used as a linked list inside the FT library.
"
Class {
	#name : #FTListRec,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_HEAD',
		'OFFSET_TAIL'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTListRec class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_ListNode  head;
    FT_ListNode  tail;
)
]

{ #category : #'accessing structure variables' }
FTListRec >> head [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_HEAD) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTListRec >> head: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_HEAD put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTListRec >> tail [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_TAIL) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTListRec >> tail: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_TAIL put: anObject getHandle.
]
