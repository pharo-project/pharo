testing
testInDomain
	| cookie |
	cookie := ZnCookie fromString: self cookieString for: 'http://www.google.com' asZnUrl.
	self assert: (cookie isInDomain: 'http://www.google.com' asZnUrl).
	self assert: (cookie isInDomain: 'http://google.com' asZnUrl).
	self assert: ((cookie isInDomain: 'http://pharo-project.org' asZnUrl) not)
	