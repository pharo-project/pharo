"
I'm a size_t type. 
I can change in different architectures (32/64bits)

"
Class {
	#name : #FFISizeT,
	#superclass : #FFIIntegerType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFISizeT class >> externalType [ 
	^ FFIArchitecture forCurrentArchitecture externalSizeTType
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
	^ aHandle platformSizeTAt: index
]

{ #category : #private }
FFISizeT >> basicHandle: aHandle at: index put: value [
	^ aHandle platformSizeTAt: index put: value
]

{ #category : #'private - emitting code' }
FFISizeT >> emitSelector [
	^ 'platformSizeTAt'
]

{ #category : #accessing }
FFISizeT >> offsetPointerReadFieldAt: offsetVariableName [
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalSizeTType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }
]

{ #category : #accessing }
FFISizeT >> offsetReadFieldAt: offsetVariableName [
	self isPointer ifTrue: [ 
		^ self offsetPointerReadFieldAt: offsetVariableName ].
	
	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << offsetVariableName ]
]

{ #category : #accessing }
FFISizeT >> offsetWriteFieldAt: offsetVariableName with: valueName [
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			offsetWriteFieldAt: offsetVariableName 
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << offsetVariableName 
			<< ' put: ' << valueName ]
]

{ #category : #accessing }
FFISizeT >> pointerReadFieldAt: byteOffset [
	"since offsetPointerReadFieldAt: receives a variable no matter what we use the trick of 
	 just passing the offset as a string... it will work :)"
	^ self offsetPointerReadFieldAt: byteOffset asString
]

{ #category : #accessing }
FFISizeT >> readFieldAt: byteOffset [
	self isPointer ifTrue: [ 
		^ self pointerReadFieldAt: byteOffset ].

	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << byteOffset asString ].

]

{ #category : #accessing }
FFISizeT >> writeFieldAt: byteOffset with: valueName [

	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			writeFieldAt: byteOffset
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << byteOffset asString
			<< ' put: ' << valueName ]

]
