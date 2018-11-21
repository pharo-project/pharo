"
A generic (abstract) integer type.
"
Class {
	#name : #FFIIntegerType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIIntegerType >> defaultReturnOnError [

	^ 0
]

{ #category : #'stack parameter classification' }
FFIIntegerType >> stackValueParameterClass [
	^ #integer
]
