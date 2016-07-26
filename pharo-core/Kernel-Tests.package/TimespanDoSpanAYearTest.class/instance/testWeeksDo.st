tests
testWeeksDo
	| weeks weekArray |
	weeks := aTimespan weeks.
	self assert: weeks size = ((aDuration days / 7.0) ceiling + 1).

	weekArray := OrderedCollection new.
	weekArray
		addLast: (Week starting: (DateAndTime year: 2004 month: 12 day: 19) duration: 7 days);
		addLast: (Week starting: (DateAndTime year: 2004 month: 12 day: 26) duration: 7 days).

	2 to: 79 by: 7 do:
		[ :i | weekArray
				addLast: (Week starting: (DateAndTime year: 2005 day: i) duration: 7 days) ].

	weekArray := weekArray asArray.
	self assert: aTimespan weeks = weekArray
