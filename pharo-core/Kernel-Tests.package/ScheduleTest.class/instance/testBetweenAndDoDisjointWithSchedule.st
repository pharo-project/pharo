tests
testBetweenAndDoDisjointWithSchedule
	| count |
	count := 0.
	aSchedule
		between: (DateAndTime
				year: 2004
				month: 4
				day: 1)
		and: (DateAndTime
				year: 2004
				month: 4
				day: 30)
		do: [:each | count := count + 1].
	self assert: count = 0