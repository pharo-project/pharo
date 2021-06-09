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
	objectCount := self nextEncodedUint32.
	indexStream := FLIndexStream
		on: stream
		digits: objectCount bytesCount.
	objects := Array new: objectCount.
	objectsWriteStream := WriteStream on: objects.
	isBigEndian := self nextEncodedUint32
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
	"SmallInteger is 31 / 63 bits. Hence, we can store a full 24 bit signed integer.
	Look at the high bit to see whether this is actually a negative integer and then
	restore the remaining two's complement bits."
	| uInt |
	uInt := self nextEncodedUint24.
	^ (uInt bitShift: -23) = 1
		ifTrue: [ uInt + (-1 bitShift: 24) ]
		ifFalse: [ uInt ]
]

{ #category : #decoding }
FLDecoder >> nextEncodedInt32 [
	"SmallInteger is 31 / 63 bits.
	Look at the high bit (bit 30, not 31!) to see whether this is actually a negative
	integer and then restore the remaining two's complement bits.
	Note: we don't dispatch to #nextEncodedUint32 to avoid overflow to large integers
			when decoding negative integers."
	| n |
	n := stream next.
	"See whether bit 7 of the byte is set (= bit 31)"
	(n bitAnd: 64) = 64 ifTrue: [
		n := n + (-1 bitShift: 7) ].
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ (n bitShift: 8) + stream next
]

{ #category : #decoding }
FLDecoder >> nextEncodedInt64 [
	"SmallInteger is 31 / 63 bits.
	Look at the high bit (bit 61, not 63!) to see whether this is actually a negative
	integer and then restore the remaining two's complement bits.
	Note: we don't dispatch to #nextEncodedUint64 to avoid overflow to large integers
			when decoding negative integers.
	Note: while the documentation says that SmallInteger uses 63 bits on 64 bit platforms
			`SmallInteger minVal` actually only uses 61 bits!"
	| n |
	n := stream next.
	"See whether bit 5 of the byte is set (= bit 61)"
	(n bitAnd: 16) = 16 ifTrue: [
		n := n + (-1 bitShift: 5) ].
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ (n bitShift: 8) + stream next
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
	^ (n bitShift: 8) + stream next

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint32 [
	"Answer the next unsigned, 32-bit integer from this (binary) stream."

	| n |
	n := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	^ (n bitShift: 8) + stream next

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
	^ (n bitShift: 8) + stream next

]

{ #category : #decoding }
FLDecoder >> nextEncodedUint8 [
	"Answer the next unsigned, 16-bit integer from this (binary) stream."
	^ stream next
	

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
