testing
testWriting
	| requestLine string |
	requestLine := ZnRequestLine method: 'GET' uri: '/foo/bar/xyz.txt'.
	string := String streamContents: [ :stream | requestLine writeOn: stream ].
	self assert: string = ('GET /foo/bar/xyz.txt HTTP/1.1', String crlf)