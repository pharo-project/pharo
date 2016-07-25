initialize-release
readHeaderFrom: stream
	self statusLine: (ZnStatusLine readFrom: stream).
	super readHeaderFrom: stream
	