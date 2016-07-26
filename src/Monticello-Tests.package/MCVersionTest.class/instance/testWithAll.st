tests
testWithAll
	self 
		assert: #withAllDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c a)