testing
testRelative
	| url |
	url := 'http://api.foo.com:8080' asZnUrl.
	self assert: url isAbsolute.
	url parseFrom: '/user/1?format=full'.
	self assert: url printString = 'http://api.foo.com:8080/user/1?format=full'