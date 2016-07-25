testing
testQuery
	| url |
	url := 'http://foo.com/test?q' asZnUrl.
	self assert: url printString = 'http://foo.com/test?q'