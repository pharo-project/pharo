settings
manageHttpProxyExceptions
	^ LastHTTPProxyExceptions ifNil:[ LastHTTPProxyExceptions := ''].
