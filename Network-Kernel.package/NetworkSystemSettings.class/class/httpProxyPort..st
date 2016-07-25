settings
httpProxyPort: aPortNumber
	"Set the HTTP Proxy port to use to aPortNumber, use nil to reset"
	
	aPortNumber = HTTPProxyPort ifTrue: [ ^ self ].
	LastHTTPProxyPort := HTTPProxyPort.
	HTTPProxyPort := aPortNumber
