testing
testAccessors
	| session |
	session := ZnUserAgentSession new.
	self assert: (session cookieJar isKindOf: ZnCookieJar).
	self assert: (session credentials notNil)