"
I'm a constant handle type (see ==FFIConstantHandle==  comment).
"
Class {
	#name : #FFIConstantHandleType,
	#superclass : #FFIExternalReferenceType,
	#category : 'UnifiedFFI-Types'
}

{ #category : #private }
FFIConstantHandleType class >> naturalPointerArity [
	^ 0
]

{ #category : #'emitting code' }
FFIConstantHandleType >> emitReturnArgument: builder context: aContext [
	self loader 
		emitHandleArityUnpack: builder
		type: self
		context: aContext
		objectClass: self objectClass
		instVarName: self instanceVariableName
]

{ #category : #accessing }
FFIConstantHandleType >> externalType [
	^ ExternalType ulong
]

{ #category : #accessing }
FFIConstantHandleType >> externalTypeSize [ 
	^ self externalType byteSize
]

{ #category : #accessing }
FFIConstantHandleType >> instanceVariableName [
	^ #handle
]

{ #category : #'emitting code' }
FFIConstantHandleType >> offsetReadFieldAt: offsetVariableName [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super offsetReadFieldAt: offsetVariableName ].
	^ self offsetReadHandleAt: offsetVariableName
]

{ #category : #'emitting code' }
FFIConstantHandleType >> offsetReadHandleAt: offsetVariableName [
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			nextPutAll: offsetVariableName;
			nextPutAll:')' ].

]

{ #category : #'emitting code' }
FFIConstantHandleType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << offsetVariableName << ' put: anObject value' ]
]

{ #category : #'emitting code' }
FFIConstantHandleType >> readFieldAt: byteOffset [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super readFieldAt: byteOffset ].
	^ self readHandleAt: byteOffset
]

{ #category : #'emitting code' }
FFIConstantHandleType >> readHandleAt: byteOffset [
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			print: byteOffset;
			nextPutAll:')' ].

]

{ #category : #'emitting code' }
FFIConstantHandleType >> writeFieldAt: byteOffset with: valueName [
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << byteOffset asString << ' put: anObject value' ]
]
