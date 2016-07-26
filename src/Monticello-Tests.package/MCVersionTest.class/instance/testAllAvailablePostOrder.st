tests
testAllAvailablePostOrder
	self 
		assert: #allAvailableDependenciesDo: 
		orders: #(a ((b (d e)) c)) 
		as: #(d e b c)