tests
testJulianDayNumber

	self assert: (january23rd2004 translateTo: 0) equals: (Date julianDayNumber: ((4713+2004)*365 +1323) offset: 0 hour).