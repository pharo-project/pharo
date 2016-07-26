HTTP Proxy
configureHttpProxy

	self useHTTPProxy 
		ifTrue: [ self setHttpProxy ]
		ifFalse: [ NetworkSystemSettings useHTTPProxy: false ]