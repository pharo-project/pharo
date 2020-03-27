"
I am an abstraction used by the materialization algorithm to decode the graph from a stream.
"
Class {
	#name : #FLDecoder,
	#superclass : #Object,
	#instVars : [
		'stream',
		'objects',
		'isBigEndian',
		'indexStream',
		'migrations',
		'objectsWriteStream',
		'globalEnvironment'
	],
	#category : #'Fuel-Core-Base'
}

{ #category : #'instance creation' }
FLDecoder class >> on: aStream migrations: aCollection globalEnvironment: aDictionary [
	
	^self basicNew
		initializeOn: aStream 
		migrations: aCollection 
		globalEnvironment: aDictionary;
		yourself.
]

{ #category : #accessing }
FLDecoder >> classNamed: className [ 

	^ (migrations
		detect: [:m | m sourceClassName = className ]
		ifNone: [ ^ self globalClassNamed: className ])
		targetClass.
		
]

{ #category : #decoding }
FLDecoder >> decodeYourself [
	| objectCount |
	objectCount := self nextEncodedPositiveInteger.
	indexStream := FLIndexStream
		on: stream
		digits: objectCount bytesCount.
	objects := Array new: objectCount.
	objectsWriteStream := WriteStream on: objects.
	isBigEndian := self nextEncodedPositiveInteger
]

{ #category : #accessing }
FLDecoder >> globalClassNamed: className [

	^ globalEnvironment 
		at: className
		ifAbsent: [ FLClassNotFound signalWithName: className ]
]

{ #category : #accessing }
FLDecoder >> globalEnvironment [
	"Answer a dictionary where the look up for global symbols will be done during materialization."
	
	^ globalEnvironment
]

{ #category : #initializing }
FLDecoder >> initializeOn: aStream migrations: aCollection globalEnvironment: aDictionary [ 

	self initialize.
	stream := aStream.
	migrations := aCollection.
	globalEnvironment := aDictionary.
]

{ #category : #accessing }
FLDecoder >> isBigEndian [
	^ isBigEndian
]

{ #category : #decoding }
FLDecoder >> nextEncodedBitmap [

	^ Bitmap newFromStream: stream
]

{ #category : #decoding }
FLDecoder >> nextEncodedByte [
	^stream next
]

{ #category : #decoding }
FLDecoder >> nextEncodedBytesInto: aBytesObject [
	stream next: aBytesObject basicSize into: aBytesObject 
]

{ #category : #decoding }
FLDecoder >> nextEncodedClusterClass [

	^ self class environment at: self nextEncodedString asSymbol
]

{ #category : #decoding }
FLDecoder >> nextEncodedInt24 [
	| n firstDigit |
	n := firstDigit := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	firstDigit >= 128 ifTrue: [n := -16r1000000 + n].  "decode negative 24-bit integer"
	^ n

]

{ #category : #decoding }
FLDecoder >> nextEncodedInt32 [
	| n firstDigit |
	n := firstDigit := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	firstDigit >= 128 ifTrue: [n := -16r100000000 + n].  "decode negative 32-bit integer"
	^ n

]

{ #category : #decoding }
FLDecoder >> nextEncodedPositiveInteger [
	"Read a 32-bit signed integer from the next 4 bytes"
	| s |
	s := 0.
	1 to: 4 do: [:i | s := (s bitShift: 8) + stream next].
	^ s
]

{ #category : #decoding }
FLDecoder >> nextEncodedReference [ 

	^ objects at: indexStream nextIndex
]

{ #category : #decoding }
FLDecoder >> nextEncodedString [
	| length aByteArray |

	"read the length in binary mode"
	length := stream next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + stream next]].
	aByteArray := ByteArray new: length.

	stream nextInto: aByteArray.
	^aByteArray asString.

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint16 [
	"Answer the next unsigned, 16-bit integer from this (binary) stream."
	^ (stream next bitShift: 8) + (stream next).
	
]

{ #category : #decoding }
FLDecoder >> nextEncodedUint24 [
	"Answer the next unsigned, 24-bit integer from this (binary) stream."

	| n |
	n := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ n

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint32 [
	"Answer the next unsigned, 32-bit integer from this (binary) stream."

	| n |
	n := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ n

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint64 [
	"Answer the next unsigned, 64-bit integer from this (binary) stream."

	| n |
	n := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ n

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint8 [
	"Answer the next unsigned, 16-bit integer from this (binary) stream."
	^ stream next.
	

]

{ #category : #decoding }
FLDecoder >> nextEncodedWordsInto: aWordsObject [ 
	stream fuelNextWordsInto: aWordsObject 
]

{ #category : #accessing }
FLDecoder >> objects [
	^ objects
]

{ #category : #accessing }
FLDecoder >> registerAll: someObjects [

	objectsWriteStream nextPutAll: someObjects.
]

{ #category : #accessing }
FLDecoder >> variablesMappingFor: aClass [

	| variables |
	variables := FLVariablesMapping materializing: aClass from: self.
	
	migrations 
		select: [ :m | (m targetClass == aClass) or: [ aClass inheritsFrom: m targetClass ] ]
		thenDo: [ :m | m applyTo: variables ].
		
	^ variables
	
]
