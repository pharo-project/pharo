tests - iterating
testSelectOnEmpty

	self assert: (self empty select: [:e | self fail]) isEmpty
	