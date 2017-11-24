"
Character type.
Represented as single byte on C side.
"
Class {
	#name : #FFICharacterType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFICharacterType class >> externalType [
	^ ExternalType char
]

{ #category : #accessing }
FFICharacterType class >> externalTypeAlignment [
	^ 1
]

{ #category : #accessing }
FFICharacterType class >> externalTypeSize [
	^ 1
]

{ #category : #private }
FFICharacterType >> basicHandle: aHandle at: index [
	^ aHandle signedCharAt: index
]

{ #category : #private }
FFICharacterType >> basicHandle: aHandle at: index put: value [
	^ aHandle signedCharAt: index put: value
]

{ #category : #testing }
FFICharacterType >> needsArityPacking [
	">1 because it can be a 'char *', then just roll when is 'char**' or bigger"
	^ self pointerArity > 1
]

{ #category : #testing }
FFICharacterType >> needsArityUnpacking [
	"'char **' or bigger need to be unpacked"
	^ self needsArityPacking
]

{ #category : #'stack parameter classification' }
FFICharacterType >> stackValueParameterClass [
	^ #integer
]
