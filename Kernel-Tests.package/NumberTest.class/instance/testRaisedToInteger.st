tests
testRaisedToInteger

	self 
		assert: (2 raisedToInteger: 0) = 1;
		assert: (2 raisedToInteger: 1) = 2;
		assert: (2 raisedToInteger: 4) = 16;
		assert: (0 raisedToInteger: 0) = 1;
		assert: (0 raisedToInteger: 2) = 0;
		assert: (2 raisedToInteger: -1) = (1/2);
		assert: (2 raisedToInteger: -4) = (1/16).
	
	self 
		assert: (-3 raisedTo: 0) = 1;
		assert: (-3 raisedTo: 1) = -3;
		assert: (-3 raisedTo: 2) = 9;
		assert: (-3 raisedTo: 3) = -27;
		assert: (-3 raisedTo: -2) = (1/9);
		assert: (-3 raisedTo: -3) = (-1/27).
	
	self should: [ 0 raisedTo: -1 ] raise: ZeroDivide