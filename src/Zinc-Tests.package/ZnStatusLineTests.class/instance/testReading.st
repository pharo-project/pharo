testing
testReading
	| statusLine string |
	string := 'HTTP/1.1 200 OK', String crlf.
	statusLine := ZnStatusLine readFrom: string readStream.
	self assert: statusLine code = 200.
	self assert: statusLine reason = 'OK'.
	self assert: statusLine version = ZnConstants defaultHTTPVersion 