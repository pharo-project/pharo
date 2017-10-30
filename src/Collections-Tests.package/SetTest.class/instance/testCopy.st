tests - some
testCopy
	| newFull |
	full add: 3.
	full add: 2.
	newFull := full copy.
	self assert: (full size = newFull size).
	self assert: ((full select: [:each | (newFull includes: each) not]) isEmpty).
	self assert: ((newFull select: [:each | (full includes: each) not]) isEmpty).