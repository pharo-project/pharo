tests
testDateAndTimes
	| answer |
	self assert: aSchedule dateAndTimes size  = 104.
	self assert: aSchedule dateAndTimes first = firstEvent.
	answer := true.
	aSchedule dateAndTimes do: [:each | (each dayOfWeekName = 'Saturday'
		or: [each dayOfWeekName = 'Sunday']) ifFalse: [^false]].
	self assert: answer
