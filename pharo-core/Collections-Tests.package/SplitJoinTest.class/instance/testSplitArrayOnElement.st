running
testSplitArrayOnElement
	self assert: ((1 to: 10) asArray splitOn: 4)
		equals: #(#(1 2 3) #(5 6 7 8 9 10)) asOrderedCollection
