tests
testAtEnd
	| generator |
	generator := self numbersBetween: 1 and: 3.
	self deny: generator atEnd.
	generator next.
	self deny: generator atEnd.
	generator next.
	self deny: generator atEnd.
	generator next.
	self assert: generator atEnd