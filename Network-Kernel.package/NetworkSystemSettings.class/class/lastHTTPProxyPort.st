private
lastHTTPProxyPort
	^ LastHTTPProxyPort ifNil: [ LastHTTPProxyPort := HTTPProxyPort ]
