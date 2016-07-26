running
testJoinArrayUsingArray
	self assert: ((1 to: 4) joinUsing: #(8 9))
		equals: #(1 8 9 2 8 9 3 8 9 4)