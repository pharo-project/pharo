tests - iterating
testCollectThenSelectOnEmpty

	self assert: (self empty collect: [:e | self fail] thenSelect: [:e | self fail ]) isEmpty