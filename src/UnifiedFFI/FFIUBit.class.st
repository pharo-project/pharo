"
I'm an unsigned bit.

Typically, you do not use this type directly but some struct implementations can be declared as one bit: 
struct GdkEventKey {
	...
 	guint is_modifier : 1;
};

the real problem is how platforms handle this field: 

macOS and linux take it as an uint8, windows takes it as a uint.
Then, you want to use this to model the case.
"
Class {
	#name : #FFIUBit,
	#superclass : #FFIIntegerType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIUBit class >> externalType [ 
	^ FFIArchitecture forCurrentArchitecture externalUBitType
]

{ #category : #accessing }
FFIUBit class >> externalTypeAlignment [

	^ FFIArchitecture forCurrentArchitecture bitTypeAlignment
]

{ #category : #accessing }
FFIUBit class >> externalTypeSize [
	^ FFIArchitecture forCurrentArchitecture bitTypeSize
]

{ #category : #private }
FFIUBit >> basicHandle: aHandle at: index [
	^ aHandle platformUnsignedBitAt: index
]

{ #category : #private }
FFIUBit >> basicHandle: aHandle at: index put: value [ 
	^ aHandle platformUnsignedBitAt: index put: value
]

{ #category : #'private emitting code' }
FFIUBit >> emitSelector [
	^ 'platformUnsignedBitAt'
]

{ #category : #'private emitting code' }
FFIUBit >> offsetPointerReadFieldAt: offsetVariableName [
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalUBitType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }
]

{ #category : #'emitting code' }
FFIUBit >> offsetReadFieldAt: offsetVariableName [
	self isPointer ifTrue: [ 
		^ self offsetPointerReadFieldAt: offsetVariableName ].
	
	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << offsetVariableName ]
]

{ #category : #'emitting code' }
FFIUBit >> offsetWriteFieldAt: offsetVariableName with: valueName [
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			offsetWriteFieldAt: offsetVariableName 
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << offsetVariableName 
			<< ' put: ' << valueName ]
]

{ #category : #'private emitting code' }
FFIUBit >> pointerReadFieldAt: byteOffset [
	"since offsetPointerReadFieldAt: receives a variable no matter what we use the trick of 
	 just passing the offset as a string... it will work :)"
	^ self offsetPointerReadFieldAt: byteOffset asString
]

{ #category : #'emitting code' }
FFIUBit >> readFieldAt: byteOffset [
	self isPointer ifTrue: [ 
		^ self pointerReadFieldAt: byteOffset ].

	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << byteOffset asString ].

]

{ #category : #'emitting code' }
FFIUBit >> writeFieldAt: byteOffset with: valueName [
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			writeFieldAt: byteOffset
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << byteOffset asString
			<< ' put: ' << valueName ]

]
