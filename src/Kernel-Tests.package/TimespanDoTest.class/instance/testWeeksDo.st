tests
testWeeksDo
	| weekArray |
	weekArray := OrderedCollection new.
	7
		to: 98
		by: 7
		do: [:each | weekArray
				addLast: (Week
						starting: (DateAndTime year: 2003 day: each)
						duration: 7 days)].
	weekArray := weekArray asArray.
	self assert: aTimespan weeks = weekArray
