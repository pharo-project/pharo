"
A pointer structure for test purposes
"
Class {
	#name : #FFITestPointerStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_NESTEDPOINTER',
		'OFFSET_ONE',
		'OFFSET_POINTER',
		'OFFSET_TWO'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestPointerStructure class >> fieldsDesc [
" self rebuildFieldAccessors "
	^ #(
		byte one;
		long two;
		void *pointer;
		FFITestStructure *nestedPointer;
		)
]

{ #category : #'class initialization' }
FFITestPointerStructure class >> initialize [
	"self rebuildFieldAccessors"
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> nestedPointer [
	"This method was automatically generated"
	^FFITestStructure fromHandle: (handle pointerAt: OFFSET_NESTEDPOINTER)
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> nestedPointer: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_NESTEDPOINTER put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> one [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_ONE
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> one: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_ONE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> pointer [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_POINTER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> pointer: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_POINTER put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> two [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_TWO
]

{ #category : #'accessing structure variables' }
FFITestPointerStructure >> two: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_TWO put: anObject
]
