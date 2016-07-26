tests
testModulo

	| d1 d2 d3 |
	d1 := 11.5 seconds.
	d2 := d1 \\ 3.
	self assert: d2 = (Duration nanoSeconds: 1).

	d3 := d1 \\ (3 seconds).
	self assert: d3 =  (Duration seconds: 2 nanoSeconds: 500000000).

	self assert: aDuration \\ aDuration = 
		(Duration days: 0 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0). 
	self assert: aDuration \\ 2 = 
		(Duration days: 0 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 1).
	

