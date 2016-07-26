tests
testStartDay
	Week startDay: 'Wednesday'.
	self assert: Week startDay = 'Wednesday'.
	Week startDay: 'Thursday'.
	self assert: Week startDay = 'Thursday'.

