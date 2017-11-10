"
I am ZnChunkedWriteStream, I wrap another write stream and add HTTP 1.1 chunked transfer encoding.

I write a chunk for each #nextPutAll: or #next:putAll:startingAt:
I should be wrapped in a ZnBufferedWriteStream for #next: to work.
When done, send #close or #finish to me.

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnChunkedWriteStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'extraHeaders',
		'totalSize',
		'chunkCount'
	],
	#category : #Zinc-HTTP-Streaming
}

{ #category : #'instance creation' }
ZnChunkedWriteStream class >> on: writeStream [
	^ self new
		on: writeStream;
		yourself
]

{ #category : #initialize-release }
ZnChunkedWriteStream >> binary [
	stream binary
]

{ #category : #accessing }
ZnChunkedWriteStream >> chunkCount [
	^ chunkCount
]

{ #category : #initialize-release }
ZnChunkedWriteStream >> close [
	self finish.
	stream close
]

{ #category : #private }
ZnChunkedWriteStream >> crlf [
	stream nextPutAll: (self isBinary ifTrue: [ #[ 13 10 ] ] ifFalse: [ String crlf ])
]

{ #category : #accessing }
ZnChunkedWriteStream >> extraHeaders [
	^ extraHeaders ifNil: [ extraHeaders := ZnHeaders new ]
]

{ #category : #initialize-release }
ZnChunkedWriteStream >> finish [
	self writeChunkSize: 0.
	self crlf.
	extraHeaders ifNotNil: [ extraHeaders writeOn: stream ]
]

{ #category : #initialize-release }
ZnChunkedWriteStream >> flush [
	stream flush
]

{ #category : #initialization }
ZnChunkedWriteStream >> initialize [
	super initialize.
	totalSize := 0.
	chunkCount := 0
]

{ #category : #testing }
ZnChunkedWriteStream >> isBinary [
	^ stream isBinary

]

{ #category : #testing }
ZnChunkedWriteStream >> isStream [
	^ true
]

{ #category : #accessing }
ZnChunkedWriteStream >> next: count putAll: collection [
	self 
		next: count 
		putAll: collection 
		startingAt: 1
]

{ #category : #accessing }
ZnChunkedWriteStream >> next: count putAll: collection startingAt: offset [
	self writeChunkSize: count.
	stream next: count putAll: collection startingAt: offset.
	self crlf.
	chunkCount := chunkCount + 1.
	totalSize := totalSize + count

]

{ #category : #accessing }
ZnChunkedWriteStream >> nextPut: element [
	self error: 'Wrap me into a buffered stream, because each #nexPut: will create a new chunk'
]

{ #category : #accessing }
ZnChunkedWriteStream >> nextPutAll: collection [
	self 
		next: collection size
		putAll: collection
		startingAt: 1
]

{ #category : #initialize-release }
ZnChunkedWriteStream >> on: writeStream [
	stream := writeStream
]

{ #category : #accessing }
ZnChunkedWriteStream >> position [
	^ totalSize
]

{ #category : #private }
ZnChunkedWriteStream >> writeChunkSize: size [
	stream nextPutAll: (self isBinary ifTrue: [ size printStringHex asByteArray ] ifFalse: [ size printStringHex ]).
	self crlf
]
