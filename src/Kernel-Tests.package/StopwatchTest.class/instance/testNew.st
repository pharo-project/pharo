tests
testNew

	| sw |
	sw := Stopwatch new.
	
	self 
		assert: (sw isSuspended);
		assert: (sw state = #suspended);
		deny: (sw isActive);
		assert: (sw timespans isEmpty)

