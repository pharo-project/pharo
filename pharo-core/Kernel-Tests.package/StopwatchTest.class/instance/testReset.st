tests
testReset

	| sw |
	sw := Stopwatch new.
	sw activate.
	
	sw reset.
	self 
		assert: (sw isSuspended);
		assert: (sw timespans isEmpty)
