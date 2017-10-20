tests
testContents
	| generator |
	generator := self numbersBetween: 1 and: 3.
	self assert: generator contents = #(1 2 3)