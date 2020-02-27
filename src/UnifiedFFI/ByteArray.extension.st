Extension { #name : #ByteArray }

{ #category : #'*UnifiedFFI' }
ByteArray >> packAsDoubleToArity: arity [
	"In case I sending a buffer to get the contents I need to understand this message. 
	 See AthensCairoCanvas>>getCurrentPoint as an example."
	^ self packToArity: arity 
]

{ #category : #'*UnifiedFFI' }
ByteArray >> packAsFloatToArity: arity [
	"In case I sending a buffer to get the contents I need to understand this message. 
	 See AthensCairoCanvas>>getCurrentPoint as an example."
	^ self packToArity: arity 
]

{ #category : #'*UnifiedFFI' }
ByteArray >> packToArity: arity [
	"This is complicated... I assuming this ways of calling a function: 
	 
   arity == 1: 
	-----------
	ByteArray works as pointer buffer of single pointer to something: 
	ex 1)
		buffer := ByteArray new: 4.
		self ffiCall: #( void function (int *buffer) ).
	ex 2)
		buffer := 'Hello, World' asByteArray.
		self ffiCall: #( void function (char *buffer) ).
	
	arity > 1: 
	----------
	ByteArray works as pointer to allocate results:
	ex 1)
		pointer := ByteArray new: (FFIExternalType pointerSize).
		self ffiCall: #( void function ( char **answer )).
	   
	In this case this will not work fine because content of ByteArray needs to be a 
	pointer too, and then it needs to be allocated in the heap... while this could be 
	managed, I'm puting for the moment just an error and a recommendation of using an 
	ExternalAddress.
	"
	arity = 1 ifTrue: [ ^ self ].
	self error: 'Use ExternalAddress instead!'
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformBitAt: byteOffset [

	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture bitTypeSize 
		signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformBitAt: byteOffset put: value [
	
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture bitTypeSize
		signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformLongAt: byteOffset [
	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture longTypeSize 
		signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformLongAt: byteOffset put: value [
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture longTypeSize
		signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformSizeTAt: byteOffset [
	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture sizeTTypeSize
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformSizeTAt: byteOffset put: value [
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture sizeTTypeSize
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformUnsignedBitAt: byteOffset [

	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture bitTypeSize 
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformUnsignedBitAt: byteOffset put: value [
	
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture bitTypeSize
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformUnsignedLongAt: byteOffset [
	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture longTypeSize
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> platformUnsignedLongAt: byteOffset put: value [
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture longTypeSize 
		signed: false
]

{ #category : #'*UnifiedFFI' }
ByteArray >> readString [
	^ (ExternalData fromHandle: self type: ExternalType string) readString
]

{ #category : #'*UnifiedFFI' }
ByteArray >> readStringUTF8 [
	^ (ExternalData fromHandle: self type: ExternalType string) readStringUTF8.
]

{ #category : #'*UnifiedFFI' }
ByteArray >> referenceStructAt: byteOffset length: length [
	^ FFIExternalStructureReferenceHandle 
		handle: self 
		offset: byteOffset
]

{ #category : #'*UnifiedFFI' }
ByteArray >> signedByteAt: byteOffset [
    "Return a 8bit signed integer starting at the given byte offset"
    ^self integerAt: byteOffset size: 1 signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> signedByteAt: byteOffset put: value [
	"Store a 8bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 1 signed: true
]

{ #category : #'*UnifiedFFI' }
ByteArray >> unpackHandleFromArity: arity [
	"This will 'unpack' a long (handle) from a certain arity. See #packToArity: for a better explanation."

	arity > 1 ifTrue: [ ^ self error: 'Use ExternalAddress instead!' ].
	^ self unsignedLongAt: 1

]

{ #category : #'*UnifiedFFI' }
ByteArray >> writeString: aString [
	LibC memCopy: aString to: self size: aString size
]
