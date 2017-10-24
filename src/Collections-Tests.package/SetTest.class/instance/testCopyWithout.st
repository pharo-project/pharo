tests - some
testCopyWithout
	| newFull | 
	full add: 3.
	full add: 2.
	newFull := full copyWithout: 3.
	self assert: (newFull size = (full size - 1)).
	self deny: (newFull includes: 3).
	self assert: ((newFull select: [:each | (full includes: each) not]) isEmpty).
	self assert: ((full select: [:each | (newFull includes: each) not]) = (Set with: 3)).
	