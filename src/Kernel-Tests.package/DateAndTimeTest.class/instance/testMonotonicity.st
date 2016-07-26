tests
testMonotonicity

	| t1 t2 t3 t4 |
	t1 := DateAndTime now.
	t2 := DateAndTime now.
	(Delay forMilliseconds: 1000) wait.
	t3 := DateAndTime now.
	t4 := DateAndTime now.

	self
		assert: (t1 <= t2);
		assert: (t2 < t3);
		assert: (t3 <= t4).
