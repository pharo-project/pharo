tests - printing
testPrinting

	self	
		assert: time printString = '4:02:47 am';
		assert: time intervalString =  '4 hours 2 minutes 47 seconds';
		assert: time print24 = '04:02:47';
		assert: time printMinutes = '4:02 am';
		assert: time hhmm24 = '0402'.
