tests
testDependencyOrder
	self 
		assert: #allDependenciesDo: 
		orders: #(a (b c)) 
		as: #(b c)