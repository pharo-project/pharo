"
Bool type.
Represented as single byte on C side.
"
Class {
	#name : #FFIBool,
	#superclass : #FFIExternalType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIBool class >> externalType [
	^ ExternalType bool
]

{ #category : #accessing }
FFIBool class >> externalTypeAlignment [
	^ 1
]

{ #category : #accessing }
FFIBool class >> externalTypeSize [
	^ 1
]

{ #category : #private }
FFIBool >> basicHandle: aHandle at: index [
	^ aHandle booleanAt: index
]

{ #category : #private }
FFIBool >> basicHandle: aHandle at: index put: value [
	^ aHandle booleanAt: index put: value
]

{ #category : #'stack parameter classification' }
FFIBool >> stackValueParameterClass [
	^ #integer
]
