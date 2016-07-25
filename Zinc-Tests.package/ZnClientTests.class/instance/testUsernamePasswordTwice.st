testing
testUsernamePasswordTwice
	self withServerDo: [ :server | | client |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		(client := ZnClient new) 
			url: server localUrl;
			addPathSegment: 'echo';
			username: 'foo' password: 'secret'; 
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client close ]