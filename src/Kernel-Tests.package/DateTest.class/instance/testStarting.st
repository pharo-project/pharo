tests
testStarting

	| aDateAndTime anyTime |
	anyTime := '13:12' asTime.
	aDateAndTime := DateAndTime date: january23rd2004 time: anyTime.
	self assert: (Date starting: aDateAndTime) equals: january23rd2004.  