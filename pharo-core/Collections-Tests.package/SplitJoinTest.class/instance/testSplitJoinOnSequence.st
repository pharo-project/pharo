running
testSplitJoinOnSequence
	self assert: (#(6 6 6) join: (#(3 4) split: #(1 2 3 4 5)))
		equals: #(1 2 6 6 6 5)