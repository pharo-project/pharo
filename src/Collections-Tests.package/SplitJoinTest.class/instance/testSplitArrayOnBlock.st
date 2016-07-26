running
testSplitArrayOnBlock
	self assert: ((1 to: 10) asArray splitOn: [:n| n even])
		equals: #(#(1) #(3) #(5) #(7) #(9) #()) asOrderedCollection