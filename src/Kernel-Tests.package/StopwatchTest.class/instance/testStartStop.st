tests
testStartStop

	| sw t1 t2 t3 t4 |
	sw := Stopwatch new.
	t1 := DateAndTime now.
	(Delay forMilliseconds: 10) wait.
	sw activate; activate.
	(Delay forMilliseconds: 10) wait.
	t2 := DateAndTime now.
	
	self 
		deny: (sw isSuspended);
		assert: (sw isActive);
		assert: (sw timespans size = 1);
		assert: (t1 <= sw start);
		assert: (sw start <= t2).

	(Delay forMilliseconds: 10) wait.
	t3 := DateAndTime now.
	(Delay forMilliseconds: 10) wait.
	sw suspend; suspend.
	(Delay forMilliseconds: 10) wait.
	t4 := DateAndTime now.

	self 
		assert: (sw isSuspended);
		deny: (sw isActive);
		assert: (sw timespans size = 1);
		assert: (sw end between: t3 and: t4);
		assert: (t3 <= sw end);
		assert: (sw end <= t4).
