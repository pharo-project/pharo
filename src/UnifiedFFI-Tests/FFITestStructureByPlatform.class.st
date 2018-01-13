"
A structure for test purposes
"
Class {
	#name : #FFITestStructureByPlatform,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_LONG',
		'OFFSET_POINTER'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestStructureByPlatform class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	ulong long;
	void *pointer;
	)
]

{ #category : #'accessing structure variables' }
FFITestStructureByPlatform >> long [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_LONG
]

{ #category : #'accessing structure variables' }
FFITestStructureByPlatform >> long: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_LONG put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructureByPlatform >> pointer [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_POINTER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FFITestStructureByPlatform >> pointer: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_POINTER put: anObject getHandle.
]
