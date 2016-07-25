testing
testUrl
	| url request input |
	url := 'http://zn.stfx.eu/zn/small.thml' asZnUrl.
	request := ZnRequest get: url.
	"The original URL is kept inside the request line object inside the request object"
	self assert: request url equals: url.
	self assert: request relativeUrl equals: url asRelativeUrl.
	self assert: request absoluteUrl equals: url.
	url := 'http://foo.com/foo.html' asZnUrl.
	input := 'GET /foo.html HTTP/1.1' , String crlf , 
		'Host: foo.com' , String crlf , 
		String crlf.
	request := ZnRequest readFrom: input readStream.
	self assert: request url equals: url asRelativeUrl.
	self assert: request relativeUrl equals: url asRelativeUrl.
	self assert: request absoluteUrl equals: url