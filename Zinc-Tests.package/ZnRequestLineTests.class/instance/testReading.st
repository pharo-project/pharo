testing
testReading
	| requestLine string |
	string := 'GET /foo/bar/xyz.txt HTTP/1.1', String crlf.
	requestLine := ZnRequestLine readFrom: string readStream.
	self assert: requestLine method = #GET.
	self assert: requestLine uriPathQueryFragment = '/foo/bar/xyz.txt'.
	self assert: requestLine version = ZnConstants defaultHTTPVersion 