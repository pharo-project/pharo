"
I'm a size_t type. 
I can change in different architectures (32/64bits)

WARNING: I'm just implemented 32bits!
"
Class {
	#name : #FFISizeT,
	#superclass : #FFIIntegerType,
	#category : #UnifiedFFI-Types
}

{ #category : #converting }
FFISizeT class >> asExternalTypeOn: generator [
	"We resolve size_t to a uint/ulong which may be not the case always"
	^ generator resolveType: #ulong
]

{ #category : #accessing }
FFISizeT class >> externalType [ 
	^ ExternalType long
]

{ #category : #accessing }
FFISizeT class >> externalTypeAlignment [
	^ Smalltalk wordSize
]

{ #category : #accessing }
FFISizeT class >> externalTypeSize [
	^ Smalltalk wordSize
]

{ #category : #private }
FFISizeT >> basicHandle: aHandle at: index [
	^ aHandle platformUnsignedLongAt: index
]

{ #category : #private }
FFISizeT >> basicHandle: aHandle at: index put: value [
	^ aHandle platformUnsignedLongAt: index put: value
]

{ #category : #'emitting code' }
FFISizeT >> readFieldAt: byteOffset [
	self isPointer ifTrue: [ 
		^ self pointerReadFieldAt: byteOffset ].

	^ String streamContents: [ :stream |
		stream << '^handle platformUnsignedLongAt: ' << byteOffset asString ].

]

{ #category : #'emitting code' }
FFISizeT >> writeFieldAt: byteOffset with: valueName [
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			writeFieldAt: byteOffset
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle platformUnsignedLongAt: ' << byteOffset asString
			<< ' put: ' << valueName ]

]
