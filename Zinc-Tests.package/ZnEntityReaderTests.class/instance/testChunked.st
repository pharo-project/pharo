testing
testChunked
	| input headers reader entity |
	input := '3', String crlf,
		'con', String crlf,
		'8', String crlf,
		'sequence', String crlf,
		'0', String crlf, String crlf.
	(headers := ZnHeaders new)
		at: 'Content-Type' put: 'text/plain';
		at: 'Transfer-Encoding' put: 'chunked'.
	(reader := ZnEntityReader new)
		headers: headers;
		stream: input asByteArray readStream.
	entity := reader readEntity.
	self assert: entity contents = 'consequence'