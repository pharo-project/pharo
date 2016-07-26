tests
testDoWith
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan
				starting: aDate
				duration: 7 days).
	self assert: count = 13