"
I define the type for FFIExternalArray objects.
"
Class {
	#name : 'FFIExternalArrayType',
	#superclass : 'FFIExternalReferenceType',
	#category : 'UnifiedFFI-Types',
	#package : 'UnifiedFFI',
	#tag : 'Types'
}

{ #category : 'accessing' }
FFIExternalArrayType >> instanceVariableName [
	^ #handle
]
