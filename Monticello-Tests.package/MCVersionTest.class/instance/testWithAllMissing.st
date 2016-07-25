tests
testWithAllMissing
	self 
		assert: #withAllDependenciesDo: 
		orders: #(a ((b (d e)) (c missing))) 
		as: #(d e b a)