private
lastHTTPProxyServer
	^ LastHTTPProxyServer ifNil: [ LastHTTPProxyServer := HTTPProxyServer ]
