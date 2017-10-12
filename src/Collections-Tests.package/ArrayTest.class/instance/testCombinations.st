tests
testCombinations

	self assert: #(1 2 3) combinations = #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3)).
	self assert: (1 to: 3) combinations = #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3)).