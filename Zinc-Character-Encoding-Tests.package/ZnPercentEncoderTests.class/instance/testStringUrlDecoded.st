testing
testStringUrlDecoded
	self assert: ('foo%20bar' urlDecoded) equals: 'foo bar' 