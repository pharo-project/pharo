tests - iterating
testRejectThenDoOnEmpty

	self assert: (self empty reject: [:e | self fail ] thenDo: [ self fail ]) isEmpty