"
A generic (abstract) float type.
"
Class {
	#name : #FFIFloatType,
	#superclass : #FFIExternalType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIFloatType >> callbackReturnOn: callbackContext for: aFloat [
	^ callbackContext floatResult: aFloat
]

{ #category : #'stack parameter classification' }
FFIFloatType >> stackValueParameterClass [
	^ #float
]
