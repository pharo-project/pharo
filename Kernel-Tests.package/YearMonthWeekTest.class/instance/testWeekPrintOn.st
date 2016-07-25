tests
testWeekPrintOn
	| aWeek cs rw |
	aWeek := Week starting: (DateAndTime year: 1900 month: 12 day: 31).
	cs := 'a Week starting: 1900-12-30T00:00:00+00:00'.
	rw := String new writeStream.
	aWeek printOn: rw.
	self assert: rw contents = cs