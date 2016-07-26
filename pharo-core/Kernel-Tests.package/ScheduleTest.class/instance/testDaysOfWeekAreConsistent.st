tests
testDaysOfWeekAreConsistent
	| originalDayOfWeekSet modifiedDayOfWeekSet start end |
	originalDayOfWeekSet := (aSchedule dateAndTimes collect: [ :each | each dayOfWeek ]) asSet.
	start := DateAndTime
		year: 2003
		month: 1
		day: 5
		hour: 20
		minute: 30.
	end := DateAndTime
		year: 2003
		month: 6
		day: 4
		hour: 20
		minute: 30.
	modifiedDayOfWeekSet := OrderedCollection new.
	aSchedule between: start and: end do: [ :eachStart | modifiedDayOfWeekSet add: eachStart dayOfWeek ].
	self assert: originalDayOfWeekSet equals: modifiedDayOfWeekSet asSet