tests - comparing
testComparing
	self assert: 'foo' < 'foo:'.
	self assert: 'foo' < 'fooBar'.
	self assert: 'foo' <= 'foo:'.
	self assert: 'foo' <= 'fooBar'.
	self assert: 'foo:' > 'foo'.
	self assert: 'fooBar' > 'foo'.
	self assert: 'foo:' >= 'foo'.
	self assert: 'fooBar' >= 'foo'