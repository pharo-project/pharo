tests
testEquality
	
	self 
		assert: (a key = b key);
		deny: (a value = b value);
		deny: (a = b)

