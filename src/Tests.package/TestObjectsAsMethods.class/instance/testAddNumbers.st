testing
testAddNumbers

	self assert: (self add: 3 with: 4) = 7.
	self assert: (self perform: #add:with: withArguments: #(3 4)) = 7.