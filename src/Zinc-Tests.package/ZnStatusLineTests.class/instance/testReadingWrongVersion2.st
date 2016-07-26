testing
testReadingWrongVersion2
	| string |
	string := 'HTTP/0.9 Foo OK', String crlf.
	self should: [ ZnStatusLine readFrom: string readStream ] raise: ZnUnknownHttpVersion