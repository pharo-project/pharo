tests - iterating
testSelectThenDoOnEmpty

	self assert: (self empty select: [:e | self fail ] thenDo: [ self fail ]) isEmpty