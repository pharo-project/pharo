tests
testFractionPart

	self 
		assert: 2 fractionPart = 0;
		assert: (1/2) fractionPart = (1/2);
		assert: (4/3) fractionPart = (1/3);
		assert: 2.0 fractionPart = 0.0;
		assert: 0.5 fractionPart = 0.5;
		assert: 2.5 fractionPart = 0.5
