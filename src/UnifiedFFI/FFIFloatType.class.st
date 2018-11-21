"
A generic (abstract) float type.
"
Class {
	#name : #FFIFloatType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIFloatType >> callbackReturnOn: callbackContext for: aFloat [
	^ callbackContext floatResult: aFloat
]

{ #category : #accessing }
FFIFloatType >> defaultReturnOnError [

	^ 0.0
]

{ #category : #'stack parameter classification' }
FFIFloatType >> stackValueParameterClass [
	^ #float
]
