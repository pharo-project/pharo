"
I have some primitives used when using objects in FFI calls.
I have them here, because they are really unsafe and never should be called directly.
"
Class {
	#name : #PointerUtils,
	#superclass : #Object,
	#category : #'ThreadedFFI-Utils'
}

{ #category : #private }
PointerUtils class >> objectForOop: anAddressAsInteger [

	^ self primObjectForOop: anAddressAsInteger asInteger
]

{ #category : #private }
PointerUtils class >> oopForObject: anObject [

	^ ExternalAddress new
		fromInteger: (self primOopForObject: anObject);
		yourself
]

{ #category : #private }
PointerUtils class >> oopForObjectForcePinInMemory: anObject [

	anObject pinInMemory.
	^ self oopForObject: anObject
]

{ #category : #private }
PointerUtils class >> primObjectForOop: anObject [

	"This primitive fails if the object is not pinned"

	<primitive: 'primitiveGetObjectFromAddress'>

	^ self primitiveFailed
]

{ #category : #private }
PointerUtils class >> primOopForObject: anObject [

	"This primitive fails if the object is not pinned"

	<primitive: 'primitiveGetAddressOfOOP'>

	anObject isPinned ifFalse: [ self error: 'Objects passed by reference should be pinned' ].

	^ self primitiveFailed
]
