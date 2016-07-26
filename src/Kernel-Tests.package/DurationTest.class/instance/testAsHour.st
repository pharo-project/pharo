tests
testAsHour
	|full half quarter|
	full := (Duration minutes: 60).
	half := (Duration minutes: 30).
	quarter := (Duration minutes: 15).	
	self 
		assert: 1 hour = full;
		assert: 1.0 hour = full;
		assert: 0.5 hour = half; 
		assert: (1/2) hour = half;
		assert: (1/4) hour = quarter