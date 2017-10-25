tests
testReset
	| generator |
	generator := self numbersBetween: 1 and: 3.
	self assert: generator next = 1.
	self assert: generator next = 2.
	generator reset.
	self assert: generator next = 1.
	self assert: generator next = 2.
	self assert: generator next = 3.
	self assert: generator next = nil.
	generator reset.
	self assert: generator next = 1