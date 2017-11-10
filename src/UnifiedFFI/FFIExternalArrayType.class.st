"
I define the type for FFIExternalArray objects.
"
Class {
	#name : #FFIExternalArrayType,
	#superclass : #FFIExternalReferenceType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIExternalArrayType >> instanceVariableName [
	^ #handle
]
