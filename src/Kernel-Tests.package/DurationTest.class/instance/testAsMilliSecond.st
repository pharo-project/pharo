tests
testAsMilliSecond 
	  
 	self 
		assert: 1 milliSecond = (1/1000) second;
		assert: (1/2) milliSecond = (1/2000) second;
		assert: 0.5 milliSecond = (1/2000) second;
		assert: 500 milliSecond = (1/2) second