"
I am ZnBufferedWriteStream.
I wrap a write stream and add buffering.

Make sure to always send me #flush or #close when you're done,
otherwise the last buffer might not yet have been written.
My class side's #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBufferedWriteStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'position'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #'instance creation' }
ZnBufferedWriteStream class >> on: writeStream [
	^ self basicNew
		on: writeStream;
		yourself
]

{ #category : #convenience }
ZnBufferedWriteStream class >> on: writeStream do: block [
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result
]

{ #category : #private }
ZnBufferedWriteStream >> buffer [
	buffer isNil 
		ifTrue: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer
]

{ #category : #accessing }
ZnBufferedWriteStream >> bufferFreeSize [
	^ self bufferSize - position
]

{ #category : #accessing }
ZnBufferedWriteStream >> bufferSize [
	^ buffer isNil 
		ifTrue: [ self defaultBufferSize ]
		ifFalse: [ buffer size ]
]

{ #category : #'initialize-release' }
ZnBufferedWriteStream >> close [
	self flushBuffer.
	stream close
]

{ #category : #testing }
ZnBufferedWriteStream >> closed [
	
	^ stream closed
]

{ #category : #accessing }
ZnBufferedWriteStream >> cr [
	self nextPut: Character cr
]

{ #category : #accessing }
ZnBufferedWriteStream >> crlf [
	self cr; lf
]

{ #category : #accessing }
ZnBufferedWriteStream >> defaultBufferSize [
	^ 2 raisedToInteger: 16
]

{ #category : #private }
ZnBufferedWriteStream >> discardBuffer [

	position := 0
]

{ #category : #'initialize-release' }
ZnBufferedWriteStream >> finish [
	self flushBuffer
]

{ #category : #accessing }
ZnBufferedWriteStream >> flush [
	self flushBuffer.
	stream flush
]

{ #category : #private }
ZnBufferedWriteStream >> flushBuffer [
	position = 0 ifTrue: [ ^ self ].
	position = self bufferSize
		ifTrue: [
			stream nextPutAll: buffer ]
		ifFalse: [
			(stream respondsTo: #next:putAll:startingAt:)
				ifTrue: [ stream next: position putAll: buffer startingAt: 1 ]
				ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ] ].
	position := 0
]

{ #category : #private }
ZnBufferedWriteStream >> flushBufferIfFull [
	position = self bufferSize 
		ifTrue: [ self flushBuffer ]

]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> int16: integer [
	^ self nextIntegerOfSize: 2 signed: true bigEndian: true put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> int32: integer [
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> int8: integer [
	^ self nextIntegerOfSize: 1 signed: true bigEndian: true put: integer
]

{ #category : #testing }
ZnBufferedWriteStream >> isBinary [

	^ stream isBinary
]

{ #category : #testing }
ZnBufferedWriteStream >> isStream [

	^ true
]

{ #category : #accessing }
ZnBufferedWriteStream >> lf [
	self nextPut: Character lf
]

{ #category : #accessing }
ZnBufferedWriteStream >> next: count putAll: collection [
	"Write count elements from collection"
	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
]

{ #category : #accessing }
ZnBufferedWriteStream >> next: count putAll: collection startingAt: offset [
	"Write count elements from collection starting at offset."
	
	self flushBufferIfFull.
	count <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + count with: collection startingAt: offset.
			position := position + count ]
		ifFalse: [
			self flushBuffer.
			count > (self bufferSize / 2)
				ifTrue: [ stream next: count putAll: collection startingAt: offset ]
				ifFalse: [ self next: count putAll: collection startingAt: offset ] ]

]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> nextInt32Put: integer [
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian put: value [
	"Assuming the receiver is a stream of bytes, write value as the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, encode as a two-complement signed value, 
	else encode as a plain unsigned value."
	
	| unsignedValue |
	unsignedValue := (signed and: [ value negative ])
		ifTrue: [ (1 << (numberOfBytes * 8)) + value ] 
		ifFalse: [ value ].
	(unsignedValue between: 0 and: (2 ** (numberOfBytes * 8)) - 1)
		ifFalse: [ DomainError signalFrom: 0 to: (2 ** (numberOfBytes * 8)) - 1 ].
	bigEndian
		ifTrue: [ 
			numberOfBytes to: 1 by: -1 do: [ :index |
				self nextPut: (unsignedValue byteAt: index) ] ]
		ifFalse: [ 
			1 to: numberOfBytes do: [ :index |
				self nextPut: (unsignedValue byteAt: index) ] ].
	^ value
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> nextLittleEndianNumber: numberOfBytes put: integer [
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> nextNumber: numberOfBytes put: integer [
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: true put: integer
]

{ #category : #accessing }
ZnBufferedWriteStream >> nextPut: object [
	self flushBufferIfFull.
	position := position + 1.
	self buffer at: position put: object
]

{ #category : #accessing }
ZnBufferedWriteStream >> nextPutAll: collection [
	"Write a collection"
	
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> nextWordPut: integer [
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
]

{ #category : #'initialize-release' }
ZnBufferedWriteStream >> on: writeStream [
	stream := writeStream.
	position := 0
]

{ #category : #accessing }
ZnBufferedWriteStream >> position [

	^ stream position + position
]

{ #category : #accessing }
ZnBufferedWriteStream >> position: anInteger [ 
	self flush.
	stream position: anInteger
]

{ #category : #accessing }
ZnBufferedWriteStream >> print: object [
	object printOn: self
]

{ #category : #printing }
ZnBufferedWriteStream >> printOn: aStream [
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
]

{ #category : #accessing }
ZnBufferedWriteStream >> setToEnd [
	
	self flush.
	stream setToEnd
]

{ #category : #accessing }
ZnBufferedWriteStream >> sizeBuffer: size [
	buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size
]

{ #category : #accessing }
ZnBufferedWriteStream >> space [
	self nextPut: Character space
]

{ #category : #accessing }
ZnBufferedWriteStream >> tab [
	self nextPut: Character tab
]

{ #category : #accessing }
ZnBufferedWriteStream >> truncate [
	
	stream truncate
]

{ #category : #accessing }
ZnBufferedWriteStream >> truncate: anInteger [ 
	
	self flushBuffer.
	stream truncate: anInteger
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> uint16: integer [
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> uint32: integer [
	^ self nextIntegerOfSize: 4 signed: false bigEndian: true put: integer
]

{ #category : #'accessing-bytes' }
ZnBufferedWriteStream >> uint8: integer [
	^ self nextIntegerOfSize: 1 signed: false bigEndian: true put: integer
]
