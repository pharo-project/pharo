tests
testDoWith
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan starting: jan01 duration: aDay).
	self assert: count = 7