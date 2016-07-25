tests
testQuotient

	| d1 d2 q |
	d1 := 11.5 seconds.
	d2 := d1 // 3.
	self assert: d2 = (Duration seconds: 3 nanoSeconds: 833333333).

	q := d1 // (3 seconds).
	self assert: q = 3.

