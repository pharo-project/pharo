tests
testBetweenAndDoOverlappingSchedule
	| count |
	count := 0.
	aSchedule
		between: (DateAndTime
				year: 2002
				month: 12
				day: 1)
		and: (DateAndTime
				year: 2003
				month: 1
				day: 31)
		do: [:each | count := count + 1].
	self assert: count = 8