testing
testLiteralDynamicArray
	| source tree1 |
	source := 'foo ^ { ''film'' . ''FILM''} '.
	tree1 := RBParser parseMethod: source.
	self assert: ((self formatterClass new format: tree1) occurrencesOf: $.) = 1