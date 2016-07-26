tests
testTimeZoneEquivalence
  "DateAndTimeTest new testTimeZoneEquivalence"
	"When the clock on the wall in Detroit says 9:00am, the clock on the wall
	in London says 2:00pm. The Duration difference between the corresponding
	DateAndTime values should be zero."
	
	 " Detroit is 5 hours behind UTC, this offset to UTC is therefore written with a minus sign. This example tests both the correct interpretation of the DateAndTime denotation and correct DateAndTime arithmetics. "

	| twoPmInLondon nineAmInDetroit durationDifference |
	twoPmInLondon := '2004-11-02T14:00:00+00:00' asDateAndTime.
	nineAmInDetroit  := '2004-11-02T09:00:00-05:00' asDateAndTime.
	durationDifference := twoPmInLondon - nineAmInDetroit.
	self assert: durationDifference asSeconds = 0.
	self assert: twoPmInLondon = nineAmInDetroit
