"
I am ZnChunkedReadStream, implementing HTTP 1.1 chunked transfer encoding on a wrapped streams.

Clients should read me until I am atEnd.

After I am completely read, I can tell you my totalSize and optional extraHeaders.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnChunkedReadStream,
	#superclass : #Stream,
	#instVars : [
		'stream',
		'chunk',
		'position',
		'limit',
		'atEnd',
		'totalSize',
		'chunkCount',
		'extraHeaders'
	],
	#category : #'Zinc-HTTP-Streaming'
}

{ #category : #'instance creation' }
ZnChunkedReadStream class >> new [
	^ super new initialize
]

{ #category : #'instance creation' }
ZnChunkedReadStream class >> on: stream [
	^ self basicNew
		initialize;
		on: stream;
		yourself
]

{ #category : #testing }
ZnChunkedReadStream >> atEnd [
	self ensureChunkOrAtEnd.
	^ atEnd
]

{ #category : #accessing }
ZnChunkedReadStream >> binary [
	stream binary
]

{ #category : #accessing }
ZnChunkedReadStream >> chunkCount [
	^ chunkCount
]

{ #category : #accessing }
ZnChunkedReadStream >> collectionSpecies [
	^ self isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]
	
]

{ #category : #accessing }
ZnChunkedReadStream >> contents [
	"This is technically not correct, but it is better than nothing"
	
	^ self upToEnd
]

{ #category : #private }
ZnChunkedReadStream >> ensureChunkBufferOfSize: size [
	(chunk notNil and: [ size <= chunk size ]) ifTrue: [ ^ self ].  
	chunk := self collectionSpecies new: size.

]

{ #category : #private }
ZnChunkedReadStream >> ensureChunkOrAtEnd [
	atEnd 
		ifTrue: [ ^ self ].
	(chunk isNil or: [ position >= limit ]) 
		ifTrue: [ self getNextChunk ]

]

{ #category : #accessing }
ZnChunkedReadStream >> extraHeaders [
	^ extraHeaders
]

{ #category : #private }
ZnChunkedReadStream >> getExtraHeaders [
	extraHeaders := ZnHeaders readFrom: stream
]

{ #category : #private }
ZnChunkedReadStream >> getNextChunk [
	| size |
	(size := self getNextChunkSize) = 0
		ifTrue: [
			atEnd := true.
			chunk := position := limit := nil.
			self getExtraHeaders ]
		ifFalse: [
			totalSize := totalSize + size.
			chunkCount := chunkCount + 1.
			self ensureChunkBufferOfSize: size.  
			stream next: size into: chunk.
			stream next: 2. "consume CRLF"
			limit := size.
			position := 0 ]
]

{ #category : #private }
ZnChunkedReadStream >> getNextChunkSize [
	| line |
	(line := (ZnLineReader on: stream) nextLine) isEmpty ifTrue: [ ^ 0 ].
	^ Integer readFrom: (line copyUpTo: $;) base: 16
]

{ #category : #initialization }
ZnChunkedReadStream >> initialize [
	atEnd := false.
	chunkCount := 0.
	totalSize := 0
]

{ #category : #testing }
ZnChunkedReadStream >> isBinary [
	^ stream isBinary

]

{ #category : #accessing }
ZnChunkedReadStream >> match: subCollection [
	"Try to read and match the elements of subCollection. 
	If successful return true and leave the me positioned after the match.
	If unsuccesful return false and leave me at end."
	
	| buffer pattern bufferPosition bufferLimit |
	pattern := subCollection readStream.
	"we have to use an internal buffer because we are not positionable"
	buffer := subCollection species new: subCollection size.
	bufferPosition := bufferLimit := 0.
	[ pattern atEnd ] whileFalse: [ | nextElement |
		self atEnd ifTrue: [ ^ false ].
		"get the next char to match either from the buffer or from the stream while buffering it"
		(bufferPosition < bufferLimit) 
			ifTrue: [ 
				nextElement := buffer at: bufferPosition + 1 ]
			ifFalse: [
				nextElement := buffer at: bufferPosition + 1 put: self next.
				bufferLimit := bufferLimit + 1 ].
		bufferPosition := bufferPosition + 1.
		pattern next = nextElement
			ifFalse: [
				"shift the buffer down one element and restart"
				2 to: bufferLimit do: [ :each |
					buffer at: each - 1 put: (buffer at: each) ].
				bufferLimit := bufferLimit - 1.
				bufferPosition := 0.
				pattern position: 0 ] ].
	^ true
]

{ #category : #accessing }
ZnChunkedReadStream >> next [
	self ensureChunkOrAtEnd.
	self atEnd ifTrue: [ ^ nil ].
	^ chunk at: (position := position + 1)
]

{ #category : #accessing }
ZnChunkedReadStream >> next: requestedCount [ 
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
]

{ #category : #accessing }
ZnChunkedReadStream >> next: requestedCount into: collection [
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self
		next: requestedCount
		into: collection
		startingAt: 1
]

{ #category : #accessing }
ZnChunkedReadStream >> next: requestedCount into: collection startingAt: offset [
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| readCount |
	readCount := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]
]

{ #category : #'initialize-release' }
ZnChunkedReadStream >> on: readStream [
	stream := readStream
]

{ #category : #accessing }
ZnChunkedReadStream >> peek [
	self ensureChunkOrAtEnd.
	self atEnd ifTrue: [ ^ nil ].
	^ chunk at: (position + 1)
]

{ #category : #accessing }
ZnChunkedReadStream >> position [
	^ self atEnd 
		ifTrue: [ totalSize ]
		ifFalse: [ totalSize - chunk size + position ]
]

{ #category : #printing }
ZnChunkedReadStream >> printOn: aStream [
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
]

{ #category : #accessing }
ZnChunkedReadStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset.
	Return the actual number of elements read."

	| read toRead |
	read := 0.
	[ read < requestedCount and: [ self atEnd not] ] whileTrue: [
		toRead := (limit - position) min: (requestedCount - read).
		collection 
			replaceFrom: offset + read 
			to: offset + read + toRead - 1 
			with: chunk 
			startingAt: position + 1.
		read := read + toRead.
		position := position + toRead ].
	^ read
]

{ #category : #initialization }
ZnChunkedReadStream >> reset [
	"We don't allow it"
]

{ #category : #accessing }
ZnChunkedReadStream >> skip: count [
	count timesRepeat: [ self next ]
]

{ #category : #accessing }
ZnChunkedReadStream >> totalSize [
	^ totalSize
]

{ #category : #accessing }
ZnChunkedReadStream >> upTo: anObject [ 
	"We use our own collectionSpecies."
	
	^ self collectionSpecies streamContents: [ :out | | element |
		[ self atEnd or: [ (element := self next) = anObject ] ]
			whileFalse: [ out nextPut: element ] ]
]

{ #category : #accessing }
ZnChunkedReadStream >> upToEnd [
	"We use our own collectionSpecies and read by chunk"

	^ self collectionSpecies streamContents: [ :out |
		[ self atEnd ] whileFalse: [ 
			out next: limit putAll: chunk.
			position := limit ] ]
]
