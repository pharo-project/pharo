tests - under design
testSymmetric
	| t1 t2 |
	t1 := DateAndTime
		year: 1
		month: 1
		day: 2
		hour: 2
		minute: 3
		second: 4
		nanoSecond: 5
		offset: 6 hours.
	t2 := DateAndTime
		year: 1
		month: 1
		day: 2
		hour: 2
		minute: 3
		second: 4
		nanoSecond: 5
		offset: 6 hours.
	self assert: t2 equals: t1.
	self assert: t1 equals: t2