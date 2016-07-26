tests
testIntegerPart

	self 
		assert: 2 integerPart = 2;
		assert: (1/2) integerPart = 0;
		assert: (4/3) integerPart = 1;
		assert: 2.0 integerPart = 2.0;
		assert: 0.5 integerPart = 0.0;
		assert: 2.5 integerPart = 2.0
