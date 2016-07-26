testing
testAsRelativeUrl
	| url |
	url := 'http://api.foo.com:8080/foo/bar.txt' asZnUrl.
	self assert: url hasHost.
	self assert: url hasScheme.
	self assert: url hasPort.
	self assert: url path equals: 'foo/bar.txt'.
	url := url asRelativeUrl.
	self deny: url hasHost.
	self deny: url hasScheme.
	self deny: url hasPort.
	self assert: url path equals: 'foo/bar.txt'.
	self assert: url asString equals: '/foo/bar.txt'