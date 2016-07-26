tests
testTimeZoneEquivalence2
  "DateAndTimeTest new testTimeZoneEquivalence2"
	"This example demonstates the fact that
        2004-05-24T22:40:00  UTC  is
        2004-05-25T01:40:00  in Moscow
     (Moscow is 3 hours ahead of UTC)  "

	| thisMoment thisMomentInMoscow |
    thisMoment := DateAndTime year: 2004 month: 5 day: 24 hour: 22 minute: 40.
    thisMomentInMoscow := thisMoment offset: 3 hours.
	self assert: (thisMoment - thisMomentInMoscow) asSeconds = 0.
	self assert: thisMoment = thisMomentInMoscow
