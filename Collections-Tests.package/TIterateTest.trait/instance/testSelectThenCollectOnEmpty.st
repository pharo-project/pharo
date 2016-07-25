tests - iterating
testSelectThenCollectOnEmpty

	self assert: (self empty select: [:e | self fail ] thenCollect: [ self fail ]) isEmpty