tests
testAllMissing
	self 
		assert: #allDependenciesDo: 
		orders: #(a ((b (d e)) (c missing))) 
		as: #(d e b)