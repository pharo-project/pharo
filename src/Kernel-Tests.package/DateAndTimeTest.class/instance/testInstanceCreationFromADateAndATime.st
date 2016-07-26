tests - instance
testInstanceCreationFromADateAndATime

	| date time instance |
	date := Date today.
	time := Time now.
	instance := DateAndTime date: date time: time.
	self assert: instance asDate equals: date.
	self assert: instance asTime equals: time.