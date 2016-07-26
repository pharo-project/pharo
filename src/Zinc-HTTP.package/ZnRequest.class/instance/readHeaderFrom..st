initialize-release
readHeaderFrom: stream
	self requestLine: (ZnRequestLine readFrom: stream).
	super readHeaderFrom: stream
	