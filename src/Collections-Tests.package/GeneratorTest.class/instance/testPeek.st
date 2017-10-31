tests
testPeek
	| generator |
	generator := self numbersBetween: 1 and: 3.
	self assert: generator peek = 1.
	self assert: generator peek = 1.
	generator next.
	self assert: generator peek = 2