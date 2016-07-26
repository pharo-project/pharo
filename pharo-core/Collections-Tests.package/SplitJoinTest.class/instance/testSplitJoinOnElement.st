running
testSplitJoinOnElement
	self assert: (0 join: (3 split: #(1 2 3 4 5)))
		equals: #(1 2 0 4 5)