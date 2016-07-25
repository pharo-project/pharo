testing
testUsernamePasswordInUserInfo
	self withServerDo: [ :server | | url client |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		url := server localUrl.
		url username: 'foo'; password: 'secret'; addPathSegment: 'echo'.
		(client := ZnClient new) 
			get: url.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Zinc').
		client close ]