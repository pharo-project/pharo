testing
testReadingWrongVersion
	| string |
	string := 'HTTP/0.9 200 OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpVersion