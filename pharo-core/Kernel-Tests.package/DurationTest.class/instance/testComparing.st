tests
testComparing

	| d1 d2 d3 |
	d1 := Duration seconds: 10 nanoSeconds: 1.
	d2 := Duration seconds: 10 nanoSeconds: 1.
	d3 := Duration seconds: 10 nanoSeconds: 2.
	
	self
		assert: (d1 = d1);
		assert: (d1 = d2);
		deny: (d1 = d3);
		assert: (d1 < d3)
