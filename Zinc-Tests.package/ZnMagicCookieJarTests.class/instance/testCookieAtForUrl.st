testing
testCookieAtForUrl
	| jar c1 c2 |
	jar := ZnCookieJar new.
	c1 := ZnCookie fromString: self cookieString for: 'http://www.google.com' asZnUrl.
	c2 := ZnCookie fromString: self cookieStringAlt for: 'http://www.pharo-project.org' asZnUrl.
	jar add: c1; add: c2.
	self assert: ((jar cookieAt: 'PREF' forUrl: 'http://www.google.com' asZnUrl) notNil).
	self assert: ((jar cookieAt: 'foobar' forUrl: 'http://www.google.com' asZnUrl) isNil)