tests
testNanoSecond	 
	self 
		assert: (Duration nanoSeconds: 5) = 5 nanoSecond;	
		assert: 0.5 nanoSecond = (Duration nanoSeconds: 0.5);	
		assert: (1/2) nanoSecond = (Duration nanoSeconds: 0.5).	
				