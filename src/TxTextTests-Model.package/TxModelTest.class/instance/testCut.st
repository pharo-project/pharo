tests-navigation
testCut
	self newText: 'ABCDEF'.
	
	selection all.
	selection end moveLeft: 2.
	selection start moveRight: 2.
	
	selection cut.

	self assert: text asString equals: 'ABEF'