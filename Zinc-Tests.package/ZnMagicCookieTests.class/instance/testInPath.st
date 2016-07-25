testing
testInPath
	| cookie |
	cookie := ZnCookie fromString: self cookieStringSubpath for: 'www.google.com' asZnUrl.
	self assert: (cookie isInPath: 'www.google.com/test/' asZnUrl).
	self assert: ((cookie isInPath: 'www.google.com' asZnUrl) not)
	