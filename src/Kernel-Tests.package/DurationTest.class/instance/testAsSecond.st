tests
testAsSecond 
	|full half quarter|
	full := (Duration seconds: 1).
	half := (Duration seconds: 0.5).
	quarter := (Duration seconds: 0.25).	
	self 
		assert: 1 second = full;
		assert: 1.0 second = full;
		assert: 0.5 second = half; 
		assert: (1/2) second = half;
		assert: (1/4) second = quarter.
	self assert: 0.4 second + 0.6 second = 1 second	