testing
testReadingWrongVersion
	| string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.5', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpVersion