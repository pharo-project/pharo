tests
testDirectReturn

	| x |
	self assert: (x := self directReturnOfValue) == 6