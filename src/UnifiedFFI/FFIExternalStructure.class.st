"
I am a deprecated class to add a backwards compatible API to FFIStructure.
FFIExternalStructure got renamed in favor of FFIStructure during Pharo8.0.

To see how to use myself, please check FFIStructure
"
Class {
	#name : 'FFIExternalStructure',
	#superclass : 'FFIStructure',
	#category : 'UnifiedFFI-Deprecated',
	#package : 'UnifiedFFI',
	#tag : 'Deprecated'
}

{ #category : 'private' }
FFIExternalStructure >> handle: aHandle at: aNumber [
	"used for boxing/unboxing, as part of the FFIValueHolder mechanism"

	^ self class basicNew setHandle: (aHandle pointerAt: aNumber)
]

{ #category : 'private' }
FFIExternalStructure >> handle: aHandle at: aNumber put: anExternalAddress [
	"used for boxing/unboxing, as part of the FFIValueHolder mechanism"
	
	aHandle pointerAt: aNumber put: anExternalAddress
]
