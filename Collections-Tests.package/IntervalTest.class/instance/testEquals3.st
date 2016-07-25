tests
testEquals3

	self assert: (3 to: 5 by: 2) first = (3 to: 6 by: 2) first.
	self assert: (3 to: 5 by: 2) last = (3 to: 6 by: 2) last.
	self assert: (3 to: 5 by: 2) = (3 to: 6 by: 2).