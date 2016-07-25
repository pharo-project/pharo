tests
testWorkDatesDo
	| count |
	count := 0.
	aTimespan
		workDatesDo: [:each | count := count + 1].
	self assert: count = 5