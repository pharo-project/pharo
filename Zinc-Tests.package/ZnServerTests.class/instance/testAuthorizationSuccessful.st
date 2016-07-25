testing
testAuthorizationSuccessful
	self withServerDo: [ :server | | response |
		server authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
		response := ZnEasy get: (server localUrl addPathSegments: #('echo' 'foo'); yourself) username: 'foo' password: 'secret'.
		self assert: (response headers contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity string includesSubstring: 'Zinc').
		self assert: (response entity string includesSubstring: 'foo') ]