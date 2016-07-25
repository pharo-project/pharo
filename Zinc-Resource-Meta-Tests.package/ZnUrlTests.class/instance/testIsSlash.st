testing
testIsSlash
	self assert: '' asZnUrl isSlash.
	self assert: '/' asZnUrl isSlash.
	self assert: '/foo' asZnUrl isSlash not.
	self assert: 'http://host/foo' asZnUrl isSlash not.