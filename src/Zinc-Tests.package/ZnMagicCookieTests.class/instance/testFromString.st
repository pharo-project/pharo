testing
testFromString
	| cookie |
	cookie := ZnCookie fromString: self cookieString for: 'www.google.com' asZnUrl.
	self assert: (cookie name = 'PREF').
	self assert: (cookie path notNil).
	self assert: (cookie domain = '.google.com').
	self assert: (cookie isExpired not)
	