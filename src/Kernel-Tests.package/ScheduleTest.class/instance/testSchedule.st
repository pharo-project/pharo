tests
testSchedule
	self assert: aSchedule schedule size = 2.
	self assert: aSchedule schedule first = 1 days.	
	self assert: aSchedule schedule second = 6 days.
