testing
testWithCookieJar
	self assert: ((ZnUserAgentSession withCookieJar: ZnCookieJar new) notNil)