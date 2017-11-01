"
I am ZnEntityReader, I help reading ZnEntities from a stream given meta data in headers.

I deal with chunking and gzip decoding.

I have several options:
	- to read streaming entities, where the client has to do the actual reading
	- to read binary entities, where textual content is not interpreted
	- to allow reading up to end, when there is no content length set

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnEntityReader,
	#superclass : #Object,
	#instVars : [
		'headers',
		'stream',
		'streaming',
		'binary',
		'allowReadingUpToEnd'
	],
	#category : 'Zinc-HTTP-Streaming'
}

{ #category : #accessing }
ZnEntityReader >> allowReadingUpToEnd [
	allowReadingUpToEnd := true
]

{ #category : #testing }
ZnEntityReader >> allowsReadingUpToEnd [
	^ allowReadingUpToEnd notNil and: [ allowReadingUpToEnd ]
]

{ #category : #accessing }
ZnEntityReader >> binary [
	binary := true
]

{ #category : #testing }
ZnEntityReader >> canReadContent [
	^ self hasContentLength or: [ self isChunked or: [ self allowsReadingUpToEnd ] ]
]

{ #category : #accessing }
ZnEntityReader >> contentLength [
	^ (self headers includesKey: 'Content-Length')
		ifTrue: [ self headers contentLength ]
		ifFalse: [ nil ]
]

{ #category : #accessing }
ZnEntityReader >> contentType [
	^ (self headers includesKey: 'Content-Type') 
		ifTrue: [ self headers contentType ] 
		ifFalse: [ ZnMimeType default ]
]

{ #category : #testing }
ZnEntityReader >> hasContentLength [
	^ self headers hasContentLength and: [ self headers contentLength > 0 ]
]

{ #category : #accessing }
ZnEntityReader >> headers [
	^ headers
]

{ #category : #accessing }
ZnEntityReader >> headers: znHeaders [
	headers := znHeaders
]

{ #category : #testing }
ZnEntityReader >> isBinary [
	^ binary notNil and: [ binary ]
]

{ #category : #testing }
ZnEntityReader >> isChunked [
	| transferEncoding |
	transferEncoding := self headers at: 'Transfer-Encoding' ifAbsent: [ ^ false ].
	^ transferEncoding = 'chunked' or: [ transferEncoding beginsWith: 'chunked' ]
]

{ #category : #testing }
ZnEntityReader >> isGzipped [
	| contentEncoding |
	contentEncoding := self headers at: 'Content-Encoding' ifAbsent: [ ^ false ].
	^ contentEncoding = 'gzip'
]

{ #category : #testing }
ZnEntityReader >> isIdentityEncoded [
	^ self isGzipped not
]

{ #category : #testing }
ZnEntityReader >> isStreaming [
	^ streaming notNil and: [ streaming ]
]

{ #category : #public }
ZnEntityReader >> readEntity [
	| entity |
	self canReadContent ifFalse: [ ^ nil ].
	entity := self readEntityFromStream.
	^ entity isEmpty
		ifTrue: [ nil ]
		ifFalse: [ entity ]    
]

{ #category : #private }
ZnEntityReader >> readEntityFromStream [
	| entity decodedEntityLength chunkedStream isChunked |
	chunkedStream := (isChunked := self isChunked) ifTrue: [ stream := ZnChunkedReadStream on: stream ] ifFalse: [ nil ].
	self isGzipped 
		ifTrue: [ 
			self hasContentLength ifTrue: [ stream := ZnLimitedReadStream on: stream limit: self contentLength ].
			decodedEntityLength := nil.
			stream := GZipReadStream on: stream ]
		ifFalse: [ decodedEntityLength := self contentLength ].
	entity := self readFrom: stream usingType: self contentType andLength: decodedEntityLength.
	isChunked ifTrue: [ 
		self isIdentityEncoded ifTrue: [ entity contentLength: stream totalSize ].
		chunkedStream extraHeaders ifNotNil: [ :extraHeaders | self headers addAll: extraHeaders ] ].
	^ entity
]

{ #category : #private }
ZnEntityReader >> readFrom: readStream usingType: contentType andLength: length [
	| entityClass selector |
	entityClass := self isStreaming 
		ifTrue: [ ZnStreamingEntity ]
		ifFalse: [ ZnEntity ].
	selector := self isBinary 
		ifTrue: [ #readBinaryFrom:usingType:andLength: ]
		ifFalse: [ #readFrom:usingType:andLength: ].
	^ entityClass perform: selector 
		with: readStream
		with: contentType 
		with: length
]

{ #category : #accessing }
ZnEntityReader >> stream [
	^ stream
]

{ #category : #accessing }
ZnEntityReader >> stream: readStream [
	stream := readStream 
]

{ #category : #accessing }
ZnEntityReader >> streaming [
	streaming := true
]
