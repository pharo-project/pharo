testing
testReadingWrongMethod
	| string |
	string := 'GETT /foo/bar/xyz.txt HTTP/1.1', String crlf.
	self should: [ ZnRequestLine readFrom: string readStream ] raise: ZnUnknownHttpMethod