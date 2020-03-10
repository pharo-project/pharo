"
I am an abstraction used by the serialization algorithm to encode the graph in a stream.
"
Class {
	#name : #FLEncoder,
	#superclass : #Object,
	#instVars : [
		'stream',
		'objectsIndexes',
		'objectCount',
		'indexStream',
		'globalEnvironment',
		'substitutionIndexes'
	],
	#category : #'Fuel-Core-Base'
}

{ #category : #'instance creation' }
FLEncoder class >> on: aStream globalEnvironment: aDictionary [
	
	^self basicNew
		initializeOn: aStream
		globalEnvironment: aDictionary;
		yourself.
]

{ #category : #'instance creation' }
FLEncoder class >> on: aStream globalEnvironment: aDictionary do: aBlock [
	
	| anEncoder |
	anEncoder := self on: aStream globalEnvironment: aDictionary.
	^ [ aBlock value: anEncoder ] ensure: [ anEncoder flush ]
]

{ #category : #encoding }
FLEncoder >> encodeBitmap: aBitmap [ 
	"Append to the receiver an Integer as the next two bytes."
	aBitmap writeOn: stream.

]

{ #category : #encoding }
FLEncoder >> encodeByte: aSmallInteger [ 
	stream nextPut: aSmallInteger
]

{ #category : #encoding }
FLEncoder >> encodeBytes: aBytesObject [
	stream nextBytesPutAll: aBytesObject 
]

{ #category : #encoding }
FLEncoder >> encodeClusterClass: aClusterClass [ 

	self encodeString: aClusterClass name
]

{ #category : #encoding }
FLEncoder >> encodeInt24: aSmallInteger [
	"Write a signed integer to the next 4 bytes"

	| pos |
	pos := aSmallInteger < 0
		ifTrue: [ (0 - aSmallInteger bitXor: 16rFFFFFF) + 1 ]
		ifFalse: [ aSmallInteger ].
	1 to: 3 do: [ :i | stream nextPut: (pos byteAt: 4 - i) ]
]

{ #category : #encoding }
FLEncoder >> encodeInt32: aSmallInteger [
	"Write a signed integer to the next 4 bytes"

	| pos |
	pos := aSmallInteger < 0
		ifTrue: [ (0 - aSmallInteger) bitInvert32 + 1 ]
		ifFalse: [ aSmallInteger ].
	1 to: 4 do: [ :i | stream nextPut: (pos byteAt: 5 - i) ]
]

{ #category : #encoding }
FLEncoder >> encodePositiveInteger: anInteger [
	"Append to the receiver an Integer as the next 4 bytes."

	1 to: 4 do: [ :i | stream nextPut: (anInteger byteAt: 4 + 1 - i) ]
]

{ #category : #encoding }
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

{ #category : #encoding }
FLEncoder >> encodeReferenceToClusterObjectClass: aClass [ 
	indexStream nextIndexPut: (objectsIndexes
		at: aClass
		ifAbsent: [ FLObjectNotFound signalWith: aClass ])
]

{ #category : #encoding }
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

{ #category : #encoding }
FLEncoder >> encodeUint16: aSmallInteger [ 
	"Append to the receiver an Integer as the next two bytes."
	stream nextPut: (aSmallInteger bitShift: -8).
	stream nextPut: (aSmallInteger bitAnd: 255).
]

{ #category : #encoding }
FLEncoder >> encodeUint24: aSmallInteger [ 
	"Append to the receiver an Integer as the next 3 bytes."
	stream		
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)


]

{ #category : #encoding }
FLEncoder >> encodeUint32: aSmallInteger [ 
	"Append to the receiver an Integer as the next 4 bytes."
	stream		
		nextPut: (aSmallInteger bitShift: -24);
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)

]

{ #category : #encoding }
FLEncoder >> encodeUint64: aSmallInteger [ 
	"Append to the receiver an Integer as the next 8 bytes."
	stream	
		nextPut: (aSmallInteger bitShift: -56);
		nextPut: ((aSmallInteger bitShift: -48) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -40) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -32) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -24) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)

]

{ #category : #encoding }
FLEncoder >> encodeUint8: aSmallInteger [ 
	"Append to the receiver an Integer as the next two bytes."
	stream nextPut: aSmallInteger 
]

{ #category : #encoding }
FLEncoder >> encodeWeakReferenceTo: anObject [

	indexStream 
		nextIndexPut: (objectsIndexes at: anObject ifAbsent: [objectsIndexes at: nil]) 

]

{ #category : #encoding }
FLEncoder >> encodeWords: aWordsObject [
	stream nextWordsPut: aWordsObject
]

{ #category : #encoding }
FLEncoder >> encodeYourself [

	self encodePositiveInteger: objectCount.
	self encodePositiveInteger: FLPlatform current isBigEndian asBit.
]

{ #category : #encoding }
FLEncoder >> flush [

	^ stream flush.
]

{ #category : #hooks }
FLEncoder >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitSubstitution: self by: nil
]

{ #category : #accessing }
FLEncoder >> globalEnvironment [
	"Answer a dictionary where the look up for global symbols will be done during serialization."
	
	^ globalEnvironment
]

{ #category : #'initialize-release' }
FLEncoder >> initializeOn: aStream globalEnvironment: aDictionary [
	
	self initialize.
	stream := FLBufferedWriteStream on: aStream.
	globalEnvironment := aDictionary.
]

{ #category : #accessing }
FLEncoder >> objectCount [
	^ objectCount
]

{ #category : #accessing }
FLEncoder >> objectCount: aNumber [
	objectCount := aNumber.
	objectsIndexes := FLLargeIdentityDictionary new.
	substitutionIndexes := FLLargeIdentityDictionary new.
	indexStream := FLIndexStream on: stream digits: aNumber bytesCount
]

{ #category : #accessing }
FLEncoder >> objectsIndexes [
	^ objectsIndexes
]

{ #category : #accessing }
FLEncoder >> substitutionIndexes [
	^ substitutionIndexes
]
