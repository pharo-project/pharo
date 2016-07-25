testing
testReadingWrongCode
	| string |
	string := 'HTTP/1.0 1000 OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpStatusCode 