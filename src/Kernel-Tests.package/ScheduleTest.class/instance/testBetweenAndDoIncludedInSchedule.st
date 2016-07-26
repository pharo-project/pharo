tests
testBetweenAndDoIncludedInSchedule
	| count |
	count := 0.
	aSchedule
		between: (DateAndTime
				year: 2003
				month: 4
				day: 1)
		and: (DateAndTime
				year: 2003
				month: 4
				day: 30)
		do: [:each | count := count + 1].
	self assert: count = 8