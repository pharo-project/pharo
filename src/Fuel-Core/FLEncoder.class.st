"
I am an abstraction used by the serialization algorithm to encode the graph in a stream.
"
Class {
	#name : 'FLEncoder',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'stream',
		'objectsIndexes',
		'objectCount',
		'indexStream',
		'globalEnvironment',
		'substitutionIndexes'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'encoding' }
FLEncoder >> encodeBitmap: aBitmap [ 
	"Append to the receiver an Integer as the next two bytes."
	aBitmap writeOn: stream.
]

{ #category : 'encoding' }
FLEncoder >> encodeByte: aSmallInteger [ 
	self encodeUint8: aSmallInteger
]

{ #category : 'encoding' }
FLEncoder >> encodeBytes: aBytesObject [
	stream nextBytesPutAll: aBytesObject
]

{ #category : 'encoding' }
FLEncoder >> encodeClusterClass: aClusterClass [ 

	self encodeString: aClusterClass name
]

{ #category : 'encoding' }
FLEncoder >> encodeInt24: aSmallInteger [
	"SmallInteger is 31 / 63 bits. Hence, we can store a full 24 bit signed integer.
	We only have to restore the two's complement of the remaining higher bits when
	we materialize."
	self encodeUint24: aSmallInteger
]

{ #category : 'encoding' }
FLEncoder >> encodeInt32: aSmallInteger [
	"SmallInteger is 31 / 63 bits. We could encode 32 bit signed integers in theory but
	that would mean that we could receive large integers as input, which shouldn't happen.
	We have to assume that we are storing a SmallInteger with 31 bits of information here.
	We can store the SmallInteger as a uint and then take care to restore potential higher
	bits when we materialze.
	Note: we have to mask the top bit because we don't know what was in there. Since
		we store byte units but SmallInteger is only 31 bits, the top bit may be 1 if
		the argument is negative and we're on a 64 bit system."
	self encodeUint32: (aSmallInteger bitAnd: 16r7FFFFFFF)
]

{ #category : 'encoding' }
FLEncoder >> encodeInt64: aSmallInteger [
	"SmallInteger is 31 / 63 bits. We could encode 64 bit signed integers in theory but
	that would mean that we could receive large integers as input, which shouldn't happen.
	We have to assume that we are storing a SmallInteger with 63 bits of information here.
	We can store the SmallInteger as a uint and then take care to restore potential higher
	bits when we materialze.
	Note: we have to mask the top bit because we don't know what was in there. Since
		we store byte units but SmallInteger is only 63 bits, the top bit may be 1 if
		the argument is negative and we're on a system > 64 bits.
	Note: while the documentation says that SmallInteger uses 63 bits on 64 bit platforms
			`SmallInteger minVal` actually only uses 61 bits!"
	self encodeUint64: (aSmallInteger bitAnd: 16r1FFFFFFFFFFFFFFF)
]

{ #category : 'encoding' }
FLEncoder >> encodeReferenceTo: anObject [
	| index |
	index := substitutionIndexes
		at: anObject
		ifAbsent: [
			objectsIndexes
				at: anObject
				ifAbsent: [ FLObjectNotFound signalWith: anObject ] ].
	indexStream nextIndexPut: index
]

{ #category : 'encoding' }
FLEncoder >> encodeReferenceToClusterObjectClass: aClass [ 
	indexStream nextIndexPut: (objectsIndexes
		at: aClass
		ifAbsent: [ FLObjectNotFound signalWith: aClass ])
]

{ #category : 'encoding' }
FLEncoder >> encodeSignature [
	self encodeBytes: self configuration signature asByteArray
]

{ #category : 'encoding' }
FLEncoder >> encodeString: aString [
	| length |
	(length := aString size) < 192
		ifTrue: [ stream nextPut: length ]
		ifFalse: [ stream nextPut: (length byteAt: 4) + 192.
			stream nextPut: (length byteAt: 3).
			stream nextPut: (length byteAt: 2).
			stream nextPut: (length byteAt: 1) ].
	stream nextBytesPutAll: aString
]

{ #category : 'encoding' }
FLEncoder >> encodeUint16: aSmallInteger [ 
	"Append to the receiver an Integer as the next 2 bytes."
	stream
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)
]

{ #category : 'encoding' }
FLEncoder >> encodeUint24: aSmallInteger [ 
	"Append to the receiver an Integer as the next 3 bytes."
	stream		
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)
]

{ #category : 'encoding' }
FLEncoder >> encodeUint32: aSmallInteger [ 
	"Append to the receiver an Integer as the next 4 bytes."
	stream		
		nextPut: ((aSmallInteger bitShift: -24) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)
]

{ #category : 'encoding' }
FLEncoder >> encodeUint64: aSmallInteger [ 
	"Append to the receiver an Integer as the next 8 bytes."
	stream	
		nextPut: ((aSmallInteger bitShift: -56) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -48) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -40) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -32) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -24) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)
]

{ #category : 'encoding' }
FLEncoder >> encodeUint8: aSmallInteger [ 
	"Append to the receiver an Integer as the next byte."
	stream nextPut: (aSmallInteger bitAnd: 255)
]

{ #category : 'encoding' }
FLEncoder >> encodeVersion [
	| version |
	version := self configuration version.
	self
		encodeByte: version major;
		encodeByte: version minor;
		encodeByte: version patch
]

{ #category : 'encoding' }
FLEncoder >> encodeWeakReferenceTo: anObject [

	indexStream 
		nextIndexPut: (objectsIndexes at: anObject ifAbsent: [objectsIndexes at: nil])
]

{ #category : 'encoding' }
FLEncoder >> encodeWords: aWordsObject [
	stream nextWordsPut: aWordsObject
]

{ #category : 'encoding' }
FLEncoder >> encodeYourself [
	self
		encodeUint32: objectCount;
		encodeUint32: EndianDetector isBigEndian asBit
]

{ #category : 'encoding' }
FLEncoder >> flush [

	^ stream flush.
]

{ #category : 'accessing' }
FLEncoder >> globalEnvironment [
	"Answer a dictionary where the look up for global symbols will be done during serialization."
	
	^ globalEnvironment
]

{ #category : 'initialization' }
FLEncoder >> initialize [
	super initialize.
	
	stream := FLBufferedWriteStream on: self context stream.
	globalEnvironment := self configuration environment.
	self context registerFinalizer: [ self flush ]
]

{ #category : 'accessing' }
FLEncoder >> objectCount [
	^ objectCount
]

{ #category : 'accessing' }
FLEncoder >> objectCount: aNumber [
	objectCount := aNumber.
	objectsIndexes := IdentityDictionary new.
	substitutionIndexes := IdentityDictionary new.
	indexStream := FLIndexStream on: stream digits: aNumber bytesCount
]

{ #category : 'accessing' }
FLEncoder >> objectsIndexes [
	^ objectsIndexes
]

{ #category : 'accessing' }
FLEncoder >> substitutionIndexes [
	^ substitutionIndexes
]
