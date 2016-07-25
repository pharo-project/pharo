tests
testMonthDurations

	| jan feb dec |
	jan := Duration month: #January.
	feb := Duration month: #February.
	dec := Duration month: #December.
	
	self 
		assert: jan = (Year current months first duration);
		assert: feb = (Year current months second duration);
		assert: dec = (Year current months last duration)

		
