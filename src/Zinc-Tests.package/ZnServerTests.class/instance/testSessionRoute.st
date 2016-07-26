testing
testSessionRoute
	self withServerDo: [ :server | | client sessionId |
		server route: 'r1'.
		self assert: server route equals: 'r1'.
		client := ZnClient new url: (server localUrl addPathSegment: #session); yourself.
		self assert: client session cookieJar cookies isEmpty.
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		sessionId := client session cookieJar cookies anyOne value.
		self assert: (client contents includesSubstring: sessionId).
		self assert: (sessionId endsWith: '.r1').
		client get.
		self assert: client isSuccess.
		self assert: client session cookieJar cookies size = 1.
		self assert: client session cookieJar cookies anyOne value equals: sessionId.
		self assert: (client contents includesSubstring: sessionId) ]
