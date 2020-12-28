"
I am an strategy to implement different backends for FFI.
"
Class {
	#name : #FFIBackend,
	#superclass : #Object,
	#type : #bytes,
	#classVars : [
		'Current'
	],
	#category : #'FFI-Kernel'
}

{ #category : #accessing }
FFIBackend class >> current [

	^ Current ifNil: [ Current := self detectFFIBackend]
]

{ #category : #accessing }
FFIBackend class >> detectFFIBackend [

	^ ((self subclasses sorted: [ :a :b | a priority > b priority ])
		detect: [ :e | e isAvailable ]
		ifNone: [ ^ NullFFIBackend ]) new
]

{ #category : #accessing }
FFIBackend class >> initialize [

	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 60
]

{ #category : #testing }
FFIBackend class >> isAvailable [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend class >> startUp: isImageStarting [
	
	isImageStarting ifFalse: [ ^ self ].
	
	Current := nil.
]

{ #category : #'instance creation' }
FFIBackend >> allocate: byteSize [
	"Primitive. Allocate an object on the external heap."
	^self subclassResponsibility
]

{ #category : #'instance creation' }
FFIBackend >> free: anExternalAddress [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> integerOfObject: anObject at: byteOffset put: value size: nBytes signed: aBoolean [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> integerOfObject: anObject at: byteOffset size: nBytes signed: aBoolean [

	^ self subclassResponsibility
]

{ #category : #'instance creation' }
FFIBackend >> loadSymbol: moduleSymbol module: module [ 

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> on: anObject float32At: offset [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> on: anObject float32At: offset put: value [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> on: anObject float64At: offset [

	^ self subclassResponsibility
]

{ #category : #accessing }
FFIBackend >> on: anObject float64At: offset put: value [

	^ self subclassResponsibility
]
