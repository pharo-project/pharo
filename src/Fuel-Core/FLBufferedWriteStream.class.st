"
FLBufferedWriteStream is a buffered write stream we use for Fuel serialization. Instead of directly using the stream provided to FLSerializer at creation time by the user, we create an instance of FLBufferedWriteStream for that stream.

MultiByteFileStream has no real buffer and goes to disk too frequently. With FLBufferedWriteStream we keep stuff in a cache and only go to disk when this is full.

The way of using it is jut FLBufferedWriteStream on: aWriteStream. For example:

FLBufferedWriteStream on: (FileDirectory default forceNewFileNamed:  'TestFile') binary

With the message #sizeBuffer: you can set the size of the buffer.

Make sure to always send #flush or #close when you're done, otherwise the last buffer might not yet have been written.

"
Class {
	#name : #FLBufferedWriteStream,
	#superclass : #Stream,
	#instVars : [
		'stream',
		'buffer',
		'position',
		'streamRespondsToNextPutAllStartingAt'
	],
	#category : 'Fuel-Core-Streams'
}

{ #category : #accessing }
FLBufferedWriteStream class >> defaultBufferSize [ 
	^ 4096
]

{ #category : #'instance creation' }
FLBufferedWriteStream class >> on: writeStream [ 

	^ self on: writeStream bufferSize: self defaultBufferSize
]

{ #category : #'instance creation' }
FLBufferedWriteStream class >> on: writeStream bufferSize: aSize [
	^ self basicNew
		initializeOn: writeStream 
		bufferSize: aSize;
		yourself
]

{ #category : #private }
FLBufferedWriteStream >> buffer [
	buffer ifNil: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer
]

{ #category : #accessing }
FLBufferedWriteStream >> bufferFreeSize [
	^ buffer size - position
]

{ #category : #'file open/close' }
FLBufferedWriteStream >> close [
	self flushBuffer.
	stream close
]

{ #category : #private }
FLBufferedWriteStream >> copyWordObjectToBuffer: aWordObject [
	| blt |
	blt := (BitBlt
		toForm: (Form new hackBits: self buffer))
		sourceForm: (Form new hackBits: aWordObject).
	blt combinationRule: Form over. "store"
	blt 
		sourceX: 0;
		sourceY: 0;
		height: aWordObject byteSize // 4;
		width: 4.
	blt 
		destX: 0;
		destY:  position // 4.
	blt copyBits
]

{ #category : #accessing }
FLBufferedWriteStream >> defaultBufferSize [
	^ 8192 "2 raisedTo: 13  "
	


]

{ #category : #accessing }
FLBufferedWriteStream >> flush [
	self flushBuffer.
	stream flush
]

{ #category : #private }
FLBufferedWriteStream >> flushBuffer [
	position = 0 ifTrue: [ ^ self ].
	position = buffer size
		ifTrue: [
			stream nextPutAll: buffer ]
		ifFalse: [
			streamRespondsToNextPutAllStartingAt
				ifTrue: [ stream next: position putAll: buffer startingAt: 1 ]
				ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ] ].
	position := 0
]

{ #category : #private }
FLBufferedWriteStream >> flushBufferIfFull [
	position = buffer size 
		ifTrue: [ self flushBuffer ]

]

{ #category : #'initialize-release' }
FLBufferedWriteStream >> initializeOn: writeStream bufferSize: aSize [

	self initialize.
	self initializeStream: writeStream.
	self sizeBuffer: aSize.
	position := 0.
]

{ #category : #'initialize-release' }
FLBufferedWriteStream >> initializeStream: aWriteStream [
	stream := aWriteStream.
	"This is ugly, but it is an optimization for #flushBuffer"
	streamRespondsToNextPutAllStartingAt := (stream respondsTo: #next:putAll:startingAt:).
]

{ #category : #writing }
FLBufferedWriteStream >> nextBytesPutAll: collection [
	self flushBufferIfFull.
	collection size <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + collection size with: collection.
			position := position + collection size ]
		ifFalse: [
			self flushBuffer.
			collection size > (self buffer size / 2)
				ifTrue: [ stream nextBytesPutAll: collection ]
				ifFalse: [ self nextBytesPutAll: collection ] ]

]

{ #category : #accessing }
FLBufferedWriteStream >> nextPut: object [
	self flushBufferIfFull.
	position := position + 1.
	self buffer at: position put: object 
]

{ #category : #accessing }
FLBufferedWriteStream >> nextPutAll: collection [
	self flushBufferIfFull.
	collection size <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + collection size with: collection.
			position := position + collection size ]
		ifFalse: [
			self flushBuffer.
			collection size > (self buffer size / 2)
				ifTrue: [ stream nextPutAll: collection ]
				ifFalse: [ self nextPutAll: collection ] ]

]

{ #category : #writing }
FLBufferedWriteStream >> nextWordsPut: aWordObject [

	| byteSize |
	byteSize := aWordObject basicSize * 4.

	"Ensure we are at bigger than the words added, with size next power-of-two"
	byteSize > buffer size ifTrue: [ self sizeBuffer: 1 << (byteSize highBit)].
	
	"BitBlt needs word-aligned access of object. Flushing the buffer is a very good idea because after the position will be zero, which is word aligned.  Word objects always have a full number of words of data to write. (otherwise they'd be variableByte objects or something :P) So as long as the size you write per instance also takes N words, the position will always be aligned (except when writing the first object of a cluster) . After that, we flush when an object larger than current buffer is encountered (statistically rare), or if the buffer is full (which we do anyways).
	Finally, we also need enough free space in the buffer"
	((position bitAnd: 3) = 0 and: [byteSize < self bufferFreeSize])
      	ifFalse: [self flushBuffer].

	self copyWordObjectToBuffer: aWordObject.
	position := position +  byteSize.
]

{ #category : #accessing }
FLBufferedWriteStream >> position [
	^ position + stream position
]

{ #category : #printing }
FLBufferedWriteStream >> printOn: aStream [
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
]

{ #category : #accessing }
FLBufferedWriteStream >> sizeBuffer: size [
    buffer ifNotNil: [self flushBuffer].
    buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size

]
