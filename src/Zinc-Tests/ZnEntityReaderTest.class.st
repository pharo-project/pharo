Class {
	#name : #ZnEntityReaderTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnEntityReaderTest >> testChunked [
	| input headers reader entity |
	input := '3' , String crlf , 'con' , String crlf , '8' , String crlf , 'sequence' , String crlf , '0' , String crlf , String crlf.
	(headers := ZnHeaders new)
		at: 'Content-Type' put: 'text/plain';
		at: 'Transfer-Encoding' put: 'chunked'.
	(reader := ZnEntityReader new)
		headers: headers;
		stream: input asByteArray readStream.
	entity := reader readEntity.
	self assert: entity contents equals: 'consequence'
]

{ #category : #testing }
ZnEntityReaderTest >> testChunkedWithExtraHeaders [
	| input headers reader entity |
	input := '3' , String crlf , 'con' , String crlf , '8' , String crlf , 'sequence' , String crlf , '0' , String crlf , 'X-Foo:bar' , String crlf , String crlf.
	(headers := ZnHeaders new)
		at: 'Content-Type' put: 'text/plain';
		at: 'Transfer-Encoding' put: 'chunked'.
	(reader := ZnEntityReader new)
		headers: headers;
		stream: input asByteArray readStream.
	entity := reader readEntity.
	self assert: entity contents equals: 'consequence'.
	self assert: (headers at: 'X-Foo') equals: 'bar'
]

{ #category : #testing }
ZnEntityReaderTest >> testReadStreaming [
	| data headers reader entity |
	data := ((1 to: 32) collect: [ :each | (0 to: 9) atRandom ]) asByteArray.
	(headers := ZnHeaders new)
		at: 'Content-Type' put: ZnMimeType applicationOctetStream printString;
		at: 'Content-Length' put: data size printString.
	(reader := ZnEntityReader new)
		headers: headers;
		stream: data readStream;
		streaming.
	entity := reader readEntity.
	self assert: entity contentType equals: ZnMimeType applicationOctetStream.
	self assert: entity contentLength equals: data size.
	self assert: entity contents equals: data
]

{ #category : #testing }
ZnEntityReaderTest >> testReadStreamingWriting [
	| data headers reader entity output |
	data := ((1 to: 32) collect: [ :each | (0 to: 9) atRandom ]) asByteArray.
	(headers := ZnHeaders new)
		at: 'Content-Type' put: ZnMimeType applicationOctetStream printString;
		at: 'Content-Length' put: data size printString.
	(reader := ZnEntityReader new)
		headers: headers;
		stream: data readStream;
		streaming.
	entity := reader readEntity.
	self assert: entity contentType equals: ZnMimeType applicationOctetStream.
	self assert: entity contentLength equals: data size.
	output := ByteArray streamContents: [ :stream | entity writeOn: stream ].
	self assert: output equals: data
]
