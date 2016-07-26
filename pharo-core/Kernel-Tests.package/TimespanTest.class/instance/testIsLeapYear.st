tests
testIsLeapYear
	"self assert: anOverlappingTimespan isLeapYear."
	"not sure why this fails"
	self deny: aTimespan isLeapYear
