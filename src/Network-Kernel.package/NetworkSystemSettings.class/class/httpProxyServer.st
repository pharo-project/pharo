settings
httpProxyServer
	"Return the hostname of the HTTP proxy server to use, a String.
	Can be empty, when no HTTP proxy should be used."
	
	^ HTTPProxyServer ifNil: [ '' ]