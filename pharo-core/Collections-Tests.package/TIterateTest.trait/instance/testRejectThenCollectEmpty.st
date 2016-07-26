tests - iterating
testRejectThenCollectEmpty

	self assert: (self empty reject: [:e | self fail ] thenCollect: [ :each| self fail ]) isEmpty