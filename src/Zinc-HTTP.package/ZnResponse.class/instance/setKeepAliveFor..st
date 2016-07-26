accessing
setKeepAliveFor: request
	(request isHttp10 and: [ request isConnectionKeepAlive ])
		ifTrue: [ self setConnectionKeepAlive ]
	