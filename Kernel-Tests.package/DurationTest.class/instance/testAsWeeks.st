tests
testAsWeeks
	|full half quarter|
	full := (Duration days: 7).	 
	half := (Duration weeks: 0.5).			
	quarter := (Duration weeks: 0.25).			
	self 
		assert: 1 weeks = full;
		assert: 1.0 weeks = full;
		assert: 0.5 weeks = half; 
		assert: (1/2) weeks = half;
		assert: (1/4) weeks = quarter.
	self assert: 1.4 weeks + 1.6 weeks = 3 weeks	