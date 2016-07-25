testing
testSessionExpired
	self withServerDo: [ :server | | client sessionId |
		client := ZnClient new url: (server localUrl addPathSegment: #session); yourself.
		self assert: client session cookieJar cookies isEmpty.
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		sessionId := client session cookieJar cookies anyOne value.
		self assert: (client contents includesSubstring: sessionId).
		"Kill the server session as if it was expired"
		server sessionManager removeSessionWithId: sessionId.
		"The client still presents the old session id but should get a new one"
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		self deny: client session cookieJar cookies anyOne value = sessionId ]
