testing
testNonComment
	self assert: (STONCStyleCommentsSkipStream on: 'foo' readStream) upToEnd equals: 'foo'.
	self assert: (STONCStyleCommentsSkipStream on: 'foo' readStream) nextLine equals: 'foo'.
	self deny: (STONCStyleCommentsSkipStream on: 'foo' readStream) atEnd.
	self assert: (STONCStyleCommentsSkipStream on: '' readStream) atEnd.