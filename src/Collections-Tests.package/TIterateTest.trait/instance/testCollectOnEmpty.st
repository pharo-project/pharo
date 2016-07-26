tests - iterating
testCollectOnEmpty
	self assert: (self empty collect: [:e | self fail]) isEmpty