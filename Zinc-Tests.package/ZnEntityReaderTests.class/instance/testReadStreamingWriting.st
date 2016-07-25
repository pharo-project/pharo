testing
testReadStreamingWriting
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
	self assert: entity contentType = ZnMimeType applicationOctetStream.
	self assert: entity contentLength = data size.
	output := ByteArray streamContents: [ :stream | entity writeOn: stream ].
	self assert: output = data