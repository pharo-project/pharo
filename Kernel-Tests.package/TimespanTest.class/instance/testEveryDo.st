tests
testEveryDo
	| count duration |
	count := 0.
	duration := 7 days.
	aTimespan
		every: duration
		do: [:each | count := count + 1].
	self assert: count = 1