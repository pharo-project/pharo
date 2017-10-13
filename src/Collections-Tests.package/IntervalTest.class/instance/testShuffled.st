tests
testShuffled

	| i is |
	i := (1 to: 20).
	is := i shuffled.
	self assert: i ~= is.
	self assert: i size equals: is size.
	self assert: (is includesAll: i).
	self assert: (i includesAll: is).