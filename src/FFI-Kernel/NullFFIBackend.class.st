"
I am a null implementation of a FFIBackend
"
Class {
	#name : #NullFFIBackend,
	#superclass : #FFIBackend,
	#category : #'FFI-Kernel'
}

{ #category : #accessing }
NullFFIBackend class >> isAvailable [

	^ true
]

{ #category : #accessing }
NullFFIBackend class >> priority [

	^ 0
]

{ #category : #'instance creation' }
NullFFIBackend >> allocate: byteSize [
	"Primitive. Allocate an object on the external heap."
	^self primitiveFailed
]

{ #category : #'instance creation' }
NullFFIBackend >> free: anExternalAddress [

	^ self primitiveFailed
]

{ #category : #accessing }
NullFFIBackend >> integerOfObject: anObject at: byteOffset put: value size: nBytes signed: aBoolean [

	^ self primitiveFailed
]

{ #category : #accessing }
NullFFIBackend >> integerOfObject: anObject at: byteOffset size: nBytes signed: aBoolean [

	^ self primitiveFailed
]

{ #category : #'instance creation' }
NullFFIBackend >> loadSymbol: moduleSymbol module: module [

	^ Error signal: 'Null FFI Backend'
]

{ #category : #callbacks }
NullFFIBackend >> lookupCallbackByAddress: address [

	^ self error: 'Null FFI backend does not know about callback in address ' , address printString
]

{ #category : #accessing }
NullFFIBackend >> on: anObject float32At: offset [

	^ self primitiveFailed
]

{ #category : #accessing }
NullFFIBackend >> on: anObject float32At: offset put: value [

	^ self primitiveFailed
]

{ #category : #accessing }
NullFFIBackend >> on: anObject float64At: offset [

	^ self primitiveFailed
]

{ #category : #accessing }
NullFFIBackend >> on: anObject float64At: offset put: value [

	^ self primitiveFailed
]
