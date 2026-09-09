"
I'm en external object type (a pointer)
"
Class {
	#name : 'FFIExternalObjectType',
	#superclass : 'FFIExternalReferenceType',
	#category : 'UnifiedFFI-Types',
	#package : 'UnifiedFFI',
	#tag : 'Types'
}

{ #category : 'initialization' }
FFIExternalObjectType >> initialize [

	super initialize.
	pointerArity := self naturalPointerArity
]

{ #category : 'accessing' }
FFIExternalObjectType >> instanceVariableName [
	^ #handle
]
