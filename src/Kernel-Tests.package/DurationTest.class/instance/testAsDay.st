tests
testAsDay
	|full half quarter|
	full := (Duration minutes: 60*24).
	half := (Duration minutes: 60*12).
	quarter := (Duration minutes: 60*6).	
	self 
		assert: 1 day  = full;
		assert: 1.0 day  = full;
		assert: 0.5 day  = half; 
		assert: (1/2) day = half;
		assert: (1/4) day = quarter.
	self assert: 0.4 day + 0.6 day = 1 day	