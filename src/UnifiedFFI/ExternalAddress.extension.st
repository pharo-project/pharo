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
ExternalAddress >> autoRelease [
	^ self class finalizationRegistry add: self
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
ExternalAddress >> pointerAutoRelease [
	"Same as #pointer (see its comment for detals), but contents are garbage collected automatically"
	^ self pointer autoRelease
]

{ #category : '*UnifiedFFI' }
ExternalAddress >> readArrayOf: aType size: count [
	"Reads and returns an array of aType of the specified size."

	| externalType |
	"resolve type if needed"
	externalType := aType isString
		ifTrue: [ FFIExternalType resolveType: aType ]
		ifFalse: [ aType ].

	"then build the array"
	^ Array streamContents: [ :strean |
		| address |
		address := self.
		1 to: count do: [ :index |
			strean nextPut: (externalType handle: address at: 1).
			address := address + externalType typeSize ] ]
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
