testing
testError
	self withServerDo: [ :server | | url response |
		self deny: server debugMode.
		url := server localUrl
					addPathSegment: #error;
					queryAt: #message put: 'Test Error';
					yourself.
		response := ZnEasy get: url.
		self deny: response isSuccess.
		self assert: response code equals: 500.
		self assert: response contentType equals: ZnMimeType textPlain.
		self assert: (response entity contents includesSubstring: 'Test Error') ]