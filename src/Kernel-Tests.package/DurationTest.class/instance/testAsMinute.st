tests
testAsMinute

	|full half quarter|
	full := (Duration seconds: 60).
	half := (Duration seconds: 30).
	quarter := (Duration seconds: 15).	
	self 
		assert: 1 minute = full;
		assert: 1.0 minute = full;
		assert: 0.5 minute = half; 
		assert: (1/2) minute = half;
		assert: (1/4) minute = quarter.
	self assert: 0.4 minute + 0.6 minute = 1 minute	