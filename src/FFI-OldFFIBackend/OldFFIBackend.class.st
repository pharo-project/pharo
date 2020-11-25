"
I am an FFIBackend using SqueakFFI plugin
"
Class {
	#name : #OldFFIBackend,
	#superclass : #FFIBackend,
	#type : #bytes,
	#category : #'FFI-OldFFIBackend'
}

{ #category : #accessing }
OldFFIBackend class >> isAvailable [ 

	^ true
]

{ #category : #accessing }
OldFFIBackend class >> priority [

	^ 1
]

{ #category : #accessing }
OldFFIBackend >> allocate: byteSize [
	"Primitive. Allocate an object on the external heap."
	<primitive:'primitiveFFIAllocate' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'instance creation' }
OldFFIBackend >> free: anExternalAddress [

	^ anExternalAddress oldFree
]

{ #category : #accessing }
OldFFIBackend >> integerOfObject: anObject at: byteOffset put: value size: nBytes signed: aBoolean [

	^ anObject
		  oldIntegerAt: byteOffset
		  put: value
		  size: nBytes
		  signed: aBoolean
]

{ #category : #accessing }
OldFFIBackend >> integerOfObject: anObject at: byteOffset size: nBytes signed: aBoolean [

	^ anObject oldIntegerAt: byteOffset size: nBytes signed: aBoolean
]

{ #category : #'instance creation' }
OldFFIBackend >> isAvailable [

	^ [ 
	  self
		  loadSymbol: 'primitiveLoadSymbolFromModule'
		  module: 'SqueakFFIPrims'.
	  true ] onErrorDo: [ false ]
]

{ #category : #'instance creation' }
OldFFIBackend >> loadSymbol: moduleSymbol module: module [ 

	<primitive: 'primitiveLoadSymbolFromModule' module: 'SqueakFFIPrims'>
	^ self primitiveFailed
]

{ #category : #accessing }
OldFFIBackend >> on: anObject float32At: offset [

	^ anObject oldFFIPrimFloat32At: offset
]

{ #category : #accessing }
OldFFIBackend >> on: anObject float32At: offset put: value [

	^ anObject oldFFIPrimFloat32At: offset put: value
]

{ #category : #accessing }
OldFFIBackend >> on: anObject float64At: offset [

	^ anObject oldFFIPrimFloat64At: offset
]

{ #category : #accessing }
OldFFIBackend >> on: anObject float64At: offset put: value [

	^ anObject oldFFIPrimFloat64At: offset put: value
]
