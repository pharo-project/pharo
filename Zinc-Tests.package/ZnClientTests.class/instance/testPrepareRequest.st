testing
testPrepareRequest
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			prepareRequest: [ :request | request setAuthorization: 'my-signature' ];
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'my-signature').
		client
			url: server localUrl;
			addPath: 'echo';
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'my-signature').
		client close ]