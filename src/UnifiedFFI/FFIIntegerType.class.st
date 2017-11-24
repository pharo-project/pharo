"
A generic (abstract) integer type.
"
Class {
	#name : #FFIIntegerType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #'stack parameter classification' }
FFIIntegerType >> stackValueParameterClass [
	^ #integer
]
