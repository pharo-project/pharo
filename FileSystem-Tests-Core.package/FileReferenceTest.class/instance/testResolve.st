tests
testResolve
	| ref |
	ref := filesystem / 'griffle'.
	self assert: ref resolve == ref