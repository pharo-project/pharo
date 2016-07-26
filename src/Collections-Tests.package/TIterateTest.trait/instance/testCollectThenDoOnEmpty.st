tests - iterating
testCollectThenDoOnEmpty

	self assert: (self empty collect: [:e | self fail] thenDo: [ self fail ]) isEmpty