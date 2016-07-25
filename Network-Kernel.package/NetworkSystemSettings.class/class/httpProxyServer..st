settings
httpProxyServer: aServerName
	"Set the hostname of the HTTP proxy server to use to aServerName.
	Can be nil or empty, when no HTTP proxy should be used."

	aServerName = HTTPProxyServer ifTrue: [ ^ self ].
	LastHTTPProxyServer := HTTPProxyServer.
	HTTPProxyServer := aServerName ifNotNil: [ aServerName trimBoth ]
