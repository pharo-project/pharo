"
An ExternalAddress is an opaque handle to objects outside Smalltalk memory (e.g., a pointer).
"
Class {
	#name : #ExternalAddress,
	#superclass : #ByteArray,
	#type : #bytes,
	#classInstVars : [
		'wordSize'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'instance creation' }
ExternalAddress class >> allocate: byteSize [
	"Primitive. Allocate an object on the external heap."
	<primitive:'primitiveFFIAllocate' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'instance creation' }
ExternalAddress class >> gcallocate: byteSize [
	"Primitive. Allocate an object on the external heap.
	The external memory will be freed when i am garbage collected.
	BEWARE: there should be no copy of self, nor any pointer to a sub part..."
	
	| externalAddress |
	externalAddress := self allocate: byteSize.
	self finalizationRegistry add: externalAddress.
	^externalAddress
]

{ #category : #'class initialization' }
ExternalAddress class >> initialize [
	wordSize := Smalltalk wordSize
]

{ #category : #'instance creation' }
ExternalAddress class >> loadSymbol: moduleSymbol module: module [ 
	<primitive: 'primitiveLoadSymbolFromModule' module: 'SqueakFFIPrims'>
	^ self primitiveFailed
]

{ #category : #'instance creation' }
ExternalAddress class >> new [
	"External addresses are either four or eight bytes long."
	^super new: wordSize
]

{ #category : #'instance creation' }
ExternalAddress class >> new: n [
	"Only create ExternalAddresses of the right size."
	^n = wordSize
		ifTrue: [super new: n]
		ifFalse: [self shouldNotImplement]
]

{ #category : #'class initialization' }
ExternalAddress class >> startUp: resuming [
	"If starting the image afresh all external addresses should be zero.
	 In addition, if the word size has changed then external addresses shoiuld be resized.
	 The two steps are combined for efficiency."
	resuming ifTrue:
		[| instances |
		 instances := self allInstances.
		 wordSize ~= Smalltalk wordSize
			ifTrue: "Implement nulling by becomming all existing instances to new (and hence null) pointers of the right size."
				[wordSize := Smalltalk wordSize.
				 instances elementsForwardIdentityTo: (instances collect: [:ea| self basicNew: wordSize])]
			ifFalse:
				[instances do: [:addr| addr beNull]]]
]

{ #category : #accessing }
ExternalAddress class >> wordSize [
	^wordSize
]

{ #category : #arithmetic }
ExternalAddress >> + offset [
	"Create an address that is offset by the given number of bytes.
	More tricky than one would think due to the FFI's handling of ExternalAddress
	as pointer to an object so that 'self unsignedLongAt: ' would dereference."

	| bytes |
	"Convert xaddr -> bytes"
	bytes := self asByteArrayPointer.
	"Update bytes using platform dependent accessors"
	self size = 4
		ifTrue: [bytes unsignedLongAt: 1 put: (bytes unsignedLongAt: 1) + offset]
		ifFalse: [bytes unsignedLongLongAt: 1 put: (bytes unsignedLongLongAt: 1) + offset].
	"Convert bytes -> xaddr"
	^bytes asExternalPointer
]

{ #category : #private }
ExternalAddress >> asByteArrayPointer [
	"Answer a ByteArray containing a copy of pointer to the contents of the receiver."
	| sz |
	^(ByteArray basicNew: (sz := self size))
		replaceFrom: 1 to: sz with: self startingAt: 1 "answers self"
]

{ #category : #private }
ExternalAddress >> asExternalPointer [
	"No need to convert."
	^self
]

{ #category : #converting }
ExternalAddress >> asInteger [
	"convert address to integer"
	^ self asByteArrayPointer integerAt: 1 size: self size signed: false
]

{ #category : #initialization }
ExternalAddress >> beNull [
	"Make the receiver a NULL pointer"
	self atAllPut: 0.
]

{ #category : #accessing }
ExternalAddress >> byteAt: byteOffset [
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset
]

{ #category : #accessing }
ExternalAddress >> byteAt: byteOffset put: value [
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset put: value
]

{ #category : #copying }
ExternalAddress >> clone [

	<primitive: 148>
	self primitiveFailed
]

{ #category : #finalization }
ExternalAddress >> finalize [
	"I am an executor (a copy) of an ExternalAddress that was just garbage collected.
	I must finalize. my mission is to free memory"
	self isNull ifTrue: [^self].
	self free
]

{ #category : #initialization }
ExternalAddress >> free [
	"Primitive. Free the object pointed to on the external heap.
	Dangerous - may break your system if the receiver hasn't been
	allocated by ExternalAddress class>>allocate:. No checks are done."
	<primitive:'primitiveFFIFree' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #converting }
ExternalAddress >> fromInteger: address [
	"set my handle to point at address."
	"Do we really need this? bf 2/21/2001 23:48"

	| sz pointer |
	sz := self size.
	pointer := ByteArray new: sz.
	pointer integerAt: 1 put: address size: sz signed: false.
	self basicAt: 1 put: (pointer byteAt: 1);
		basicAt: 2 put: (pointer byteAt: 2);
		basicAt: 3 put: (pointer byteAt: 3);
		basicAt: 4 put: (pointer byteAt: 4).
	sz = 8 ifTrue:
		[self basicAt: 5 put: (pointer byteAt: 5);
			basicAt: 6 put: (pointer byteAt: 6);
			basicAt: 7 put: (pointer byteAt: 7);
			basicAt: 8 put: (pointer byteAt: 8)]
]

{ #category : #accessing }
ExternalAddress >> isExternalAddress [
	"Return true if the receiver describes the address of an object in the outside world"
	^true
]

{ #category : #testing }
ExternalAddress >> isNull [
	"Answer true if I am a null pointer"
	1 to: self size do:[:i| (self at: i) = 0 ifFalse:[^false]].
	^true
]

{ #category : #printing }
ExternalAddress >> printOn: aStream [
	"print this as a hex address ('@ 16rFFFFFFFF') to distinguish it from ByteArrays"

	aStream nextPutAll: '@ '; nextPutAll: (self asInteger storeStringBase: 16 length: 11 padded: true)
]

{ #category : #copying }
ExternalAddress >> shallowCopy [
	"Re-implemented to avoid superclass call to #new:"
	"But superclass's shallowCopy sends basicNew: and basicNew: is ok. eem 2/21/2016 15:31"
	^self clone
]
