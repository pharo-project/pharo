tests
testDateTimeDenotation2
 	"DateAndTimeTest new testDateTimeDenotation2"
	"Moscow is 3 hours ahead UTC, this offset to UTC is therefore positive. This example tests the correct interpretation of the DateAndTime denotation."

	| lateEveningInLondon lateEveningInLocalTimeOfMoscow
	localMoscowTimeFromDenotation |
	lateEveningInLondon := DateAndTime
				year: 2004
				month: 11
				day: 30
				hour: 23
				minute: 30
				second: 0
				offset: 0 hours.
	lateEveningInLocalTimeOfMoscow := lateEveningInLondon offset: 3 hours.
	localMoscowTimeFromDenotation  := '2004-12-01T02:30:00+03:00' asDateAndTime.
	self assert:  lateEveningInLocalTimeOfMoscow = localMoscowTimeFromDenotation.