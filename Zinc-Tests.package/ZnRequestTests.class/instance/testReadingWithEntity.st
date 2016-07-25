testing
testReadingWithEntity
	| input request |
	input := 'GET /foo.html HTTP/1.1', String crlf, 
		'Host: foo.com', String crlf, 
		'Content-Length: 8', String crlf, 
		'Content-Type: text/plain', String crlf, 
		String crlf, 
		'12345678'.
	request := ZnRequest readFrom: input asByteArray readStream.
	self assert: request method equals: #GET.
	self assert: request hasHeaders.
	self assert: (request contentType matches: ZnMimeType textPlain).
	self assert: request contentLength equals: 8.
	self assert: request hasHeaders.
	self assert: request entity contents equals: '12345678'
