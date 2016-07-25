testing
testSession
	self withServerDo: [ :server | | client sessionId |
		client := ZnClient new url: (server localUrl addPathSegment: #session); yourself.
		self assert: client session cookieJar cookies isEmpty.
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		sessionId := client session cookieJar cookies anyOne value.
		self assert: (client contents includesSubstring: sessionId).
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		self assert: client session cookieJar cookies anyOne value equals: sessionId.
		self assert: (client contents includesSubstring: sessionId) ]
