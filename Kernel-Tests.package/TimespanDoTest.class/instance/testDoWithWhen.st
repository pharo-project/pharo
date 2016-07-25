tests
testDoWithWhen
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan starting: aDate duration: 7 days)
		when: [:each | count < 5].
	self assert: count = 5	
