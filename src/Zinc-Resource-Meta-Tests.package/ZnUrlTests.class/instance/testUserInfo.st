testing
testUserInfo
	| url |
	url := 'http://john:secret@www.foo.com:8888/foo/bar.txt' asZnUrl.
	self assert: url scheme equals: #http.
	self assert: url username equals: 'john'.
	self assert: url password equals: 'secret'.
	self assert: url host equals: 'www.foo.com'.
	self assert: url port equals: 8888.
	self assert: url pathSegments asArray equals: #('foo' 'bar.txt').
	url := 'http://john@www.foo.com:8888/foo/bar.txt' asZnUrl.
	self assert: url scheme equals: #http.
	self assert: url username equals: 'john'.
	self deny: url hasPassword.
	self assert: url host equals: 'www.foo.com'.
	self assert: url port equals: 8888.
	self assert: url pathSegments asArray equals: #('foo' 'bar.txt')