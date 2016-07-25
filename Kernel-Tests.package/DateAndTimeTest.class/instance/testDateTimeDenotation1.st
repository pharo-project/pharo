tests
testDateTimeDenotation1
 	"DateAndTimeTest new testDateTimeDenotation1"
	
	"Detroit is 5 hours behind UTC, this offset to UTC is therefore written with a minus sign. This example tests the correct interpretation of the DateAndTime denotation. "

	| twoPmInLondon twoPmUTCInLocalTimeOfDetroit nineAmInDetroit |
	twoPmInLondon := DateAndTime
				year: 2004
				month: 11
				day: 2
				hour: 14
				minute: 0
				second: 0
				offset: 0 hours.
	twoPmUTCInLocalTimeOfDetroit := twoPmInLondon offset: -5 hours.
	nineAmInDetroit  := '2004-11-02T09:00:00-05:00' asDateAndTime.
	self assert:  twoPmUTCInLocalTimeOfDetroit = nineAmInDetroit.
	
