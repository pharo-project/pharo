tests
testPostOrder
	self 
		assert: #allDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c)