tests
testComparing
	| t1 t2 t3 |
	t1 := self timeClass fromSeconds: 14567.		"4:02:47 am"
	t2 := self timeClass fromSeconds: 5000.		"1:23:20 am"
	t3 := self timeClass fromSeconds: 80000.		"10:13:20 pm"

	self
		assert: time = t1;
		assert: time hash = t1 hash;
		assert: time = time copy.
	self
		deny: t1 < t2;
		assert: t1 < t3.