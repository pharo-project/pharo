tests
testAllUnresolved
	self 
		assert: #allDependenciesDo:ifUnresolved: 
		orders: #(a ((b (d e)) (c missing)))
		as: #(d e b)
		unresolved: #(c)