tests
testIsLeapYear 

	| dateInLeapYear dateInNonLeapYear |
	dateInLeapYear := january23rd2004.
	dateInNonLeapYear := dateInLeapYear + 365 days.
	self assert: dateInLeapYear isLeapYear equals: true.
	self assert: dateInNonLeapYear isLeapYear equals: false.


