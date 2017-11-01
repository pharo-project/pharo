"
I am ZnEntityWriter, I help writing ZnEntities to a stream given meta data in headers.

I deal with chunking and gzip encoding.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnEntityWriter,
	#superclass : #Object,
	#instVars : [
		'headers',
		'stream'
	],
	#category : 'Zinc-HTTP-Streaming'
}

{ #category : #accessing }
ZnEntityWriter >> contentType [
	^ (self headers includesKey: 'Content-Type') 
		ifTrue: [ self headers contentType ] 
		ifFalse: [ ZnMimeType default ]
]

{ #category : #accessing }
ZnEntityWriter >> headers [
	^ headers
]

{ #category : #accessing }
ZnEntityWriter >> headers: znHeaders [
	headers := znHeaders
]

{ #category : #testing }
ZnEntityWriter >> isChunked [
	| transferEncoding |
	transferEncoding := self headers at: 'Transfer-Encoding' ifAbsent: [ ^ false ].
	^ transferEncoding = 'chunked' or: [ transferEncoding beginsWith: 'chunked' ]
]

{ #category : #testing }
ZnEntityWriter >> isGzipped [
	| contentEncoding |
	contentEncoding := self headers at: 'Content-Encoding' ifAbsent: [ ^ false ].
	^ contentEncoding = 'gzip'
]

{ #category : #accessing }
ZnEntityWriter >> stream [
	^ stream
]

{ #category : #accessing }
ZnEntityWriter >> stream: writeStream [
	stream := writeStream
]

{ #category : #public }
ZnEntityWriter >> writeEntity: entity [
	| actualStream gzipStream bufferedStream chunkedStream isChunked |
	actualStream := stream.
	chunkedStream := gzipStream := bufferedStream := nil.
	(isChunked := self isChunked)
		ifTrue: [ 
			chunkedStream := actualStream := ZnChunkedWriteStream on: actualStream ].
	self isGzipped 
		ifTrue: [ 
			gzipStream := actualStream := GZipWriteStream on: actualStream ]
		ifFalse: [ 
			(isChunked and: [ self contentType isBinary not ])
				ifTrue: [ bufferedStream := actualStream := ZnUtils bufferedWriteStreamOn: actualStream ] ].
	entity writeOn: actualStream.
	gzipStream 
		ifNotNil: [ gzipStream finish ].
	bufferedStream 
		ifNotNil: [ bufferedStream finish ].
	chunkedStream
		ifNotNil: [ chunkedStream finish ]
	
]
