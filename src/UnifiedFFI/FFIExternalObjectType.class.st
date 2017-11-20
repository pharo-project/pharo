"
I'm en external object type (a pointer)
"
Class {
	#name : #FFIExternalObjectType,
	#superclass : #FFIExternalReferenceType,
	#category : #UnifiedFFI-Types
}

{ #category : #initialization }
FFIExternalObjectType >> initialize [ 
	super initialize.
	pointerArity := self class naturalPointerArity
]

{ #category : #accessing }
FFIExternalObjectType >> instanceVariableName [
	^ #handle
]
