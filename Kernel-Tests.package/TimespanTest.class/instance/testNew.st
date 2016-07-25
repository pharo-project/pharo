tests
testNew
	self assert: Timespan new = (Timespan starting: '01-01-1901' asDate)