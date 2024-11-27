Extension { #name : 'ExternalAddress' }

{ #category : '*UnifiedFFI-private' }
ExternalAddress >> adoptAddress: anExternalAddress [
	anExternalAddress = 0 ifTrue: [ ^ self beNull ].
	1 to: self size do: [ :index |
		self basicAt: index put: (anExternalAddress at: index) ]
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> arrayOfStringsFrom: aCollection [
	"answer a char**, terminated with nil"
	| stringArray index |

	stringArray := self allocate: (FFIExternalType pointerSize * (aCollection size + 1)).
	index := 1.
	aCollection do: [ :each | 
		stringArray pointerAt: index put: (self fromString: each).
		index := index + FFIExternalType pointerSize ].
	stringArray pointerAt: index put: self null.
	^ stringArray
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> asExternalAddress [

	^ self
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> autoRelease [
	^ self class finalizationRegistry add: self
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> boolean8AtOffset: zeroBasedOffset [
	<primitive: 630>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) ~= 0
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> boolean8AtOffset: zeroBasedOffset put: value [
	<primitive: 645>
	^ self integerAt: zeroBasedOffset + 1 put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> booleanAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self boolean8AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> booleanAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self boolean8AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char16AtOffset: zeroBasedOffset [
	<primitive: 641>
	^ (self integerAt: zeroBasedOffset + 1 size: 2 signed: false) asCharacter
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char16AtOffset: zeroBasedOffset put: value [
	<primitive: 656>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 2 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char32AtOffset: zeroBasedOffset [
	<primitive: 642>
	^ (self integerAt: zeroBasedOffset + 1 size: 4 signed: false) asCharacter
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char32AtOffset: zeroBasedOffset put: value [
	<primitive: 657>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 4 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char8AtOffset: zeroBasedOffset [
	<primitive: 640>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) asCharacter
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> char8AtOffset: zeroBasedOffset put: value [
	<primitive: 655>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 1 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> copyFrom: startIndex to: endIndex [
	| result size |
	size := endIndex - startIndex + 1.
	result := ByteArray new: size.
	LibC memCopy: (self + (startIndex - 1)) to: result size: size.
	^ result
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> copyFromOffset: startZeroBasedOffset to: endZeroBasedOffset [
	^ self copyFrom: startZeroBasedOffset + 1 to: endZeroBasedOffset + 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> doubleAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float64AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> doubleAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float64AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> float32AtOffset: zeroBasedOffset [
	<primitive: 643>

	^ FFIBackend current on: self float32At: zeroBasedOffset + 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> float32AtOffset: zeroBasedOffset put: value [
	<primitive: 658>

	^ FFIBackend current on: self float32At: zeroBasedOffset + 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> float64AtOffset: zeroBasedOffset [
	<primitive: 644>

	^ FFIBackend current on: self float64At: zeroBasedOffset + 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> float64AtOffset: zeroBasedOffset put: value [

	<primitive: 659>
	^ FFIBackend current
		  on: self
		  float64At: zeroBasedOffset + 1
		  put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> floatAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float32AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> floatAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float32AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> freeArrayOfStrings [
	"this is used to free an array created with `arrayOfStringsFrom:`"
	| string index |
	
	index := 1.
	[ 
		(string := self pointerAt: index) isNull ]
	whileFalse: [ 
		string free.
		index := index + FFIExternalType pointerSize ].
	
	self free 
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> fromAddress: aNumber [
	"Answers an external address who points to aNumber"
	^ self new fromAddress: aNumber
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> fromAddress: aNumber [
	^ self fromInteger: aNumber
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> fromString: aString [
	| result utf8Encoded |
	
	utf8Encoded := aString utf8Encoded. 

	result := self allocate: utf8Encoded size + 1.
	(self assert: result isNotNil).
	result writeString: utf8Encoded.
	result unsignedByteAt: utf8Encoded size + 1 put: 0.
	^ result
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> getHandle [

	^ self
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int16AtOffset: zeroBasedOffset [
	<primitive: 634>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int16AtOffset: zeroBasedOffset put: value [
	<primitive: 649>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int32AtOffset: zeroBasedOffset [
	<primitive: 636>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int32AtOffset: zeroBasedOffset put: value [
	<primitive: 651>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int64AtOffset: zeroBasedOffset [
	<primitive: 638>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int64AtOffset: zeroBasedOffset put: value [
	<primitive: 653>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int8AtOffset: zeroBasedOffset [
	<primitive: 632>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> int8AtOffset: zeroBasedOffset put: value [
	<primitive: 647>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: true
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> loadSymbol: moduleSymbol [
	"Load global symbol (one that you can find in current VM binary)"
	^ self
		loadSymbol: moduleSymbol
		module: nil
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> loadSymbol: moduleSymbol from: moduleNameOrClass [
	^ self
		loadSymbol: moduleSymbol
		module: moduleNameOrClass asFFILibrary libraryName
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> newPointer [
	^ self allocate: self wordSize
]

{ #category : '*UnifiedFFI' }
ExternalAddress class >> null [
	^ self new
]

{ #category : '*UnifiedFFI-private' }
ExternalAddress >> packToArity: arity [
	"This will 'pack' a pointer to a certain arity.
	What does this means? Let's say I have a method who will be called with a ** arity (that's 'arity 2'):

	method: aPointer
		self ffiCall: #( method ( MyExternalObjectClass **aPointer) )

	This usually means that method will put a pointer in the address of aPointer.
	And what I actually want is this pointer. So I do Something like this:

	p := MyExternalObjectClass new.
	self mehod: p.

	And I expect to have 'p' correctly initialized on return.
	Well... tha's not so simple :)

	When compiling #method:, UnifiedFFI needs to first 'pack' the pointer (which means to
	take a pointer of a pointer of a pointer... as many pointers as arity solicited), and then,
	after call, it needs to 'unpack' all this pointers (see #unpackFromArity: method)
	"
	| rolledPointer |

	rolledPointer := self.
	"Start in 2 because first pointer is self"
	2 to: arity do: [ :index | 
		rolledPointer := rolledPointer pointer ].
	^ rolledPointer
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointer [
	"Answers a pointer to the this address.
	 Sometimes, an ExternalAddress represents a pointer, so you do not need this, but some others
	 it represents just an address. Yes, I know, difference is subtle.
	 Take this as an example:

		SQSurfaceDispatch externalNew.

	 This will answer an instance if FFIExternalStructure with an ExternalAddress as handle, but
	 this handle WILL NOT represent a pointer address, just an address.
	 This is why, later in a call where I use the surface:

		AthensCairoSurface>>#initializeForNewSession

	... I need to keep a pointer to the surface, just the external address is not enough."
	^ (ExternalAddress allocate: self size)
		pointerAt: 1 put: self;
		yourself
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointerAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self pointerAtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointerAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self pointerAtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointerAtOffset: zeroBasedOffset [
	<primitive: 639>
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: zeroBasedOffset+i)].
	^addr
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointerAtOffset: zeroBasedOffset put: value [
	<primitive: 654>
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: zeroBasedOffset+i put: (value basicAt: i)].
	^value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> pointerAutoRelease [
	"Same as #pointer (see its comment for detals), but contents are garbage collected automatically"
	^ self pointer autoRelease
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> readArrayOf: aType until: aBlock [
	"Reads an array of aType until aBlock returns true.
	 this is an util to extract arrays from answers style char ** or int*, etc.
	 Example:

		someAddress readArrayOf: #uint32 until: [ :each | each isZero ].
		someAddress readArrayOf: #'void *' until: [ :each | each isNull ].
	"

	| externalType |
	"resolve type if needed"
	externalType := aType isString
		ifTrue: [ FFIExternalType resolveType: aType ]
		ifFalse: [ aType ].

	"then build the array"
	^ Array
		streamContents: [ :array |
			| address last count |
			address := self.
			count := 0.
			[ address isNull
				or: [ last := externalType handle: address at: 1.
					count := count + 1.
					aBlock cull: last cull: count ] ]
				whileFalse: [ array nextPut: last.
					address := address + externalType typeSize ] ]
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	| dstAddress srcAddress repSize |

	self flag: #pharoTodo. "Maybe replace it with a primitive (like the one at Alien)?"
	repSize := stop - start + 1.
	dstAddress := start = 1
		ifTrue: [ self ]
		ifFalse: [ self + (start - 1) ].
	srcAddress := repStart = 1
		ifTrue: [ replacement ]
		ifFalse: [ replacement copyFrom: repStart to: repStart + repSize ].
	LibC memCopy: srcAddress to: dstAddress size: repSize
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedCharAt: byteOffset [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteOffset - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedCharAt: byteIndex put: aCharacter [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int32AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int32AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedLongLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int64AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedLongLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int64AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedShortAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int16AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> signedShortAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int16AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint16AtOffset: zeroBasedOffset [
	<primitive: 633>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint16AtOffset: zeroBasedOffset put: value [
	<primitive: 648>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint32AtOffset: zeroBasedOffset [
	<primitive: 635>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint32AtOffset: zeroBasedOffset put: value [
	<primitive: 650>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint64AtOffset: zeroBasedOffset [
	<primitive: 637>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint64AtOffset: zeroBasedOffset put: value [
	<primitive: 652>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint8AtOffset: zeroBasedOffset [
	<primitive: 631>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: false
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> uint8AtOffset: zeroBasedOffset put: value [
	<primitive: 646>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: false
]

{ #category : '*UnifiedFFI-private' }
ExternalAddress >> unpackFromArity: arity [
	"This will 'unpack' a pointer from a certain arity. See #unpackToArity: for a better explanation."
	| rolledPointer previousPointer |

	rolledPointer := self.
	"Start in 2 because first pointer is self"
	2 to: arity do: [ :index | 
		previousPointer := rolledPointer.
		rolledPointer := rolledPointer pointerAt: 1.
		previousPointer free ].
	^ rolledPointer
]

{ #category : '*UnifiedFFI-private' }
ExternalAddress >> unpackHandleFromArity: arity [
	^ (self unpackFromArity: arity) unsignedLongAt: 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedByteAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint8AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedByteAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint8AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedCharAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedCharAt: byteIndex put: aCharacter [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint32AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint32AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedLongLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint64AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedLongLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint64AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedShortAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint16AtOffset: byteIndex - 1
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> unsignedShortAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint16AtOffset: byteIndex - 1 put: value
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> value [
	^ self asInteger
]
