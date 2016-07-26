tests
testArithmetic
	| t1 t2 t3 |
	t1 := time addSeconds: 70.		"4:03:57 am"
	self
		assert: t1 hours = 4;
		assert: t1 minutes = 3;
		assert: t1 seconds = 57.

	t2 := t1 addTime: (self timeClass fromSeconds: (60*60*5)).
	self
		assert: t2 hours = 9;
		assert: t2 minutes = 3;
		assert: t2 seconds = 57.

	t3 := t2 subtractTime: (self timeClass fromSeconds: (60*60*5) + 70).
	self
		assert: t3 = time.
