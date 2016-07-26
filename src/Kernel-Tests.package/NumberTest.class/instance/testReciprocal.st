tests
testReciprocal

	self 
		assert: 1 reciprocal = 1;
		assert: 2 reciprocal = (1/2);
		assert: -1 reciprocal = -1;
		assert: -3 reciprocal = (-1/3).
		
	self should: [ 0 reciprocal ] raise: ZeroDivide