tests
testRaisedToIntegerWithFloats

	self 
		assert: (2.0 raisedToInteger: 0) = 1.0;
		assert: (2.0 raisedToInteger: 1) = 2.0;
		assert: (2.0 raisedToInteger: 4) = 16.0;
		assert: (0.0 raisedToInteger: 0) = 1.0;
		assert: (0.0 raisedToInteger: 2) = 0.0;
		assert: (2.0 raisedToInteger: -1) = 0.5;
		assert: (2.0 raisedToInteger: -4) = 0.0625.
	
	self 
		assert: (-3.0 raisedTo: 0) = 1.0;
		assert: (-3.0 raisedTo: 1) = -3.0;
		assert: (-3.0 raisedTo: 2) = 9.0;
		assert: (-3.0 raisedTo: 3) = -27.0;
		assert: (-2.0 raisedTo: -2) = 0.25;
		assert: (-2.0 raisedTo: -3) = -0.125.
	
	self should: [ 0.0 raisedTo: -1 ] raise: ZeroDivide