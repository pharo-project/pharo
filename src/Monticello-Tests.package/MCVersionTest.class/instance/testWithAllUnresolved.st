tests
testWithAllUnresolved
	self 
		assert: #withAllDependenciesDo:ifUnresolved: 
		orders: #(a ((b (d e)) (c missing)))
		as: #(d e b a)
		unresolved: #(c)