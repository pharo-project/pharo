testing
testZeroContentLength
	self withServerDo: [ :server | 
		| client |
		client := ZnClient new
			url: server localUrl / #echo;
			yourself.
		"Force a bogus zero content-length header"
		client headerAt: 'Content-Length' put: '0'.
		client get.
		self assert: client isSuccess.
		client close ]