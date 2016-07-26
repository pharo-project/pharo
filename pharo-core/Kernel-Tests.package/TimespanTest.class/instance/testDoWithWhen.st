tests
testDoWithWhen
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan starting: jan01 duration: aDay)
		when: [:each | count < 5].
	self assert: count = 5