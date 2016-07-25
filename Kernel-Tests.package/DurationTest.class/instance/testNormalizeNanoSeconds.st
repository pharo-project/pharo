tests
testNormalizeNanoSeconds
	"Subtraction of two DateAndTime values may result in a request to
	create a Duration with negative nanoseconds and positive seconds.
	The resulting Duration should be normalized, otherwise its printString
	will be invalid."

	| d t1 t2 |
	t1 := '2004-01-07T11:55:01+00:00' asDateAndTime. 
	t2 := '2004-01-07T11:55:00.9+00:00' asDateAndTime.
	d := t1 - t2. "100 millisecond difference"
	self assert: d nanoSeconds > 0.
	self assert: d seconds = 0.
	self assert: d nanoSeconds = 100000000.
	self assert: d asString = '0:00:00:00.1'.
	"Verify that other combinations produces reasonable printString values"
	self assert: (Duration seconds: 1 nanoSeconds: 100000000) printString = '0:00:00:01.1'.
	self assert: (Duration seconds: -1 nanoSeconds: -100000000) printString = '-0:00:00:01.1'.
	self assert: (Duration seconds: 1 nanoSeconds: -100000000) printString = '0:00:00:00.9'.
	self assert: (Duration seconds: -1 nanoSeconds: 100000000) printString = '-0:00:00:00.9'
