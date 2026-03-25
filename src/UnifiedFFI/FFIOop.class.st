Class {
	#name : 'FFIOop',
	#superclass : 'FFIExternalType',
	#category : 'UnifiedFFI-Types',
	#package : 'UnifiedFFI',
	#tag : 'Types'
}

{ #category : 'accessing' }
FFIOop class >> externalType [
	^ ExternalType void asPointerType
]

{ #category : 'accessing' }
FFIOop class >> externalTypeAlignment [
	^ self pointerSize
]

{ #category : 'accessing' }
FFIOop class >> externalTypeSize [
	^ Smalltalk wordSize
]

{ #category : 'private' }
FFIOop class >> naturalPointerArity [
	^ 1
]

{ #category : 'private' }
FFIOop >> basicHandle: aHandle at: index [
	^ aHandle pointerAt: index
]

{ #category : 'private' }
FFIOop >> basicHandle: aHandle at: index put: value [
	aHandle pointerAt: index put: value
]

{ #category : 'accessing' }
FFIOop >> externalTypeSize [
	^ self pointerSize
]

{ #category : 'testing' }
FFIOop >> isOop [

	^ true
]

{ #category : 'testing' }
FFIOop >> needsArityUnpacking [
	"this represent kind of pointers, so it will be unpacked if they are also rolled."
	^ self needsArityPacking
]

{ #category : 'stack parameter classification' }
FFIOop >> stackValueParameterClass [
	^ #integer
]
