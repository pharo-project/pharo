tests
testAsWeek
	|full half quarter|
	full := (Duration days: 7).	 
	half := (Duration weeks: 0.5).			
	quarter := (Duration weeks: 0.25).			
	self 
		assert: 1 week = full;
		assert: 1.0 week = full;
		assert: 0.5 week = half; 
		assert: (1/2) week = half;
		assert: (1/4) week = quarter.
	self assert: 0.4 week + 0.6 week = 1 week	