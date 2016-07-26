tests - NaN behavior
testReciprocal

	self 
		assert: 1.0 reciprocal = 1.0;
		assert: 2.0 reciprocal = 0.5;
		assert: -1.0 reciprocal = -1.0;
		assert: -2.0 reciprocal = -0.5.
		
	self should: [ 0.0 reciprocal ] raise: ZeroDivide