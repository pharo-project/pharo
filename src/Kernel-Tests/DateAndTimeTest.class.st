"
SUnit tests for date and time
"
Class {
	#name : #DateAndTimeTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
DateAndTimeTest >> classToBeTested [

	^ DateAndTime


]

{ #category : #helpers }
DateAndTimeTest >> restoreLocalTimeZoneAfter: aBlock [

	| realTimeZone |
	realTimeZone := DateAndTime localTimeZone.
	aBlock ensure: [ DateAndTime localTimeZone: realTimeZone ].
]

{ #category : #coverage }
DateAndTimeTest >> selectorsToBeIgnored [

	| private | 
	private := #( #printOn: ).
	^ super selectorsToBeIgnored, private

]

{ #category : #'tests - arithmetic' }
DateAndTimeTest >> testArithmeticAcrossDateBoundary [
	| t1 t2 |
	t1 := '2004-01-07T11:55:00+00:00' asDateAndTime.
	t2 := t1 - (42900 + 1) seconds.
	self assert: t2 equals: '2004-01-06T23:59:59+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeTest >> testAsDos [
	
	| remoteDatetime |
	self assert: '21 May 2012 3:02:44 pm' asDateAndTime asDosTimestamp equals: 16r40B57856.

	"DOS times are in local time per http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx"
	remoteDatetime := DateAndTime current offset: DateAndTime localOffset + 2 hours.
	self assert: remoteDatetime asDosTimestamp equals: remoteDatetime asLocal asDosTimestamp.
]

{ #category : #tests }
DateAndTimeTest >> testAsTime [
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 0 asDuration.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 5 hours.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: -5 hours.
	self assert: dateAndTime asTime equals: (Time hour: 1 minute: 2 second: 3).
]

{ #category : #tests }
DateAndTimeTest >> testAsTimeUTC [
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 0 asDuration.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 1 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: -5 hours.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 6 minute: 2 second: 3).
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 2 second: 3 offset: 5 hours.
	self assert: dateAndTime asTimeUTC equals: (Time hour: 24+1-5 minute: 2 second: 3).
]

{ #category : #tests }
DateAndTimeTest >> testAsUnixTimeIndependentOfTimezone [
	| datetimeWithOffset datetimeWithoutOffset |
	datetimeWithoutOffset := DateAndTime current offset: Duration zero.
	datetimeWithOffset := datetimeWithoutOffset offset: (Duration hours: 1).
	
	self assert: datetimeWithOffset asUnixTime equals: datetimeWithoutOffset asUnixTime
]

{ #category : #tests }
DateAndTimeTest >> testCreationWithOffsets [
	| dt1 dt2 |
	dt1 := DateAndTime
		year: 2222
		month: 1
		day: 22
		hour: 1
		minute: 22
		second: 33
		offset: 0 hours.
	dt2 := DateAndTime
		year: 2222
		month: 1
		day: 22
		hour: 1
		minute: 22
		second: 33
		offset: 2 hours.

	"The timepoints are diffferent, AKA their UTC times don't correspond"
	self deny: dt1 equals: dt2.

	"The relative components however are equal"
	self assert: dt1 year equals: dt2 year.
	self assert: dt1 month equals: dt2 month.
	self assert: dt1 day equals: dt2 day.

	self assert: dt1 hours equals: dt2 hours.
	self assert: dt1 minutes equals: dt2 minutes.
	self assert: dt1 seconds equals: dt2 seconds
]

{ #category : #tests }
DateAndTimeTest >> testDateTimeDenotation1 [
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
	nineAmInDetroit := '2004-11-02T09:00:00-05:00' asDateAndTime.
	self assert: twoPmUTCInLocalTimeOfDetroit equals: nineAmInDetroit
]

{ #category : #tests }
DateAndTimeTest >> testDateTimeDenotation2 [
	"DateAndTimeTest new testDateTimeDenotation2"

	"Moscow is 3 hours ahead UTC, this offset to UTC is therefore positive. This example tests the correct interpretation of the DateAndTime denotation."

	| lateEveningInLondon lateEveningInLocalTimeOfMoscow localMoscowTimeFromDenotation |
	lateEveningInLondon := DateAndTime
		year: 2004
		month: 11
		day: 30
		hour: 23
		minute: 30
		second: 0
		offset: 0 hours.
	lateEveningInLocalTimeOfMoscow := lateEveningInLondon offset: 3 hours.
	localMoscowTimeFromDenotation := '2004-12-01T02:30:00+03:00' asDateAndTime.
	self assert: lateEveningInLocalTimeOfMoscow equals: localMoscowTimeFromDenotation
]

{ #category : #tests }
DateAndTimeTest >> testDayOfWeekWithUTC [
	| date |
	"Calculating the day of week should take into account the UTC offset"
	date := DateAndTime julianDayNumber: 2456385 offset:(Duration hours:2).
	"Internally, this date gets represented as the julian day 2456384 with seconds 79200 and offset 2 hours"
	"When asking for the day of week, the offset should be taken into account to return the correct day of week"
	self assert: date dayOfWeek equals: 3.
	
]

{ #category : #'tests - epoch' }
DateAndTimeTest >> testDosEpoch [
	self
		useNonUtcTimeZoneDuring: [ | localEpoch |
			localEpoch := '1 January 1980 00:00' asDateAndTime.
			self deny: DateAndTime dosEpoch equals: localEpoch ].

	self
		useTimeZone: 'UTC'
		during: [ | localEpoch |
			localEpoch := '1 January 1980 00:00' asDateAndTime.
			self assert: DateAndTime dosEpoch equals: localEpoch ].

	self assert: DateAndTime dosEpoch equals: '1980-01-01T00:00:00+00:00' asDateAndTime
]

{ #category : #'tests - bogus date' }
DateAndTimeTest >> testErrorWhenDayIsAfterMonthEnd [
	self should: [ DateAndTime year: 2004 month: 2 day: 30 ] raise: Error.
	DateAndTime year: 2004 month: 2 day: 29
]

{ #category : #'tests - bogus date' }
DateAndTimeTest >> testErrorWhenDayIsBeforeMonthStart [
	self should: [ DateAndTime year: 2004 month: 2 day: -1 ] raise: Error.
	self should: [ DateAndTime year: 2004 month: 2 day: 0 ] raise: Error.
	DateAndTime year: 2004 month: 2 day: 1
]

{ #category : #tests }
DateAndTimeTest >> testFromDos [
	
	| aDateAndTime |
	aDateAndTime := DateAndTime fromDosTimestamp: 16r40B57856.
	self assert: aDateAndTime equals:  '21 May 2012 3:02:44 pm' asDateAndTime.
	
	"DOS times are in local time per http://blogs.msdn.com/b/oldnewthing/archive/2003/09/05/54806.aspx"
	self assert: aDateAndTime offset equals: DateAndTime localOffset
]

{ #category : #tests }
DateAndTimeTest >> testFromString [
	| fromString fromStringNoOffset |
	fromString := DateAndTime fromString: '-1199-01-05T20:33:14.321-05:00'.
	self assert: fromString printString equals: '-1199-01-05T20:33:14.321-05:00'.

	"if no offset is provided, the local offset should be used"
	fromStringNoOffset := DateAndTime fromString: '-1199-01-05T20:33:14.321'.
	self assert: fromStringNoOffset offset equals: DateAndTime localOffset
]

{ #category : #'tests - instance' }
DateAndTimeTest >> testInstanceCreation [
	| t |
	t := DateAndTime
		year: 1
		month: 1
		day: 2
		hour: 2
		minute: 3
		second: 4
		nanoSecond: 5
		offset: 6 hours.
	self
		assert: t julianDayNumber equals: 1721427;
		assert: t offset equals: 6 hours;
		assert: t hour equals: 2;
		assert: t minute equals: 3;
		assert: t second equals: 4;
		assert: t nanoSecond equals: 5
]

{ #category : #'tests - instance' }
DateAndTimeTest >> testInstanceCreationFromADateAndATime [

	| date time instance |
	date := Date today.
	time := Time now.
	instance := DateAndTime date: date time: time.
	self assert: instance asDate equals: date.
	self assert: instance asTime equals: time.
]

{ #category : #tests }
DateAndTimeTest >> testMonotonicity [

	| t1 t2 t3 t4 |
	t1 := DateAndTime now.
	t2 := DateAndTime now.
	(Delay forMilliseconds: 1000) wait.
	t3 := DateAndTime now.
	t4 := DateAndTime now.

	self
		assert: (t1 <= t2);
		assert: (t2 < t3);
		assert: (t3 <= t4).

]

{ #category : #'tests - under design' }
DateAndTimeTest >> testNotSymmetricWithString [
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
	t2 := '0001-01-02T02:03:04.000000005+06:00'.
	self deny: t1 equals: t2.
	self deny: t2 equals: t1
]

{ #category : #tests }
DateAndTimeTest >> testOffset [
	| dateAndTime1 dateAndTime2 |
	dateAndTime1 := DateAndTime
		year: 1000
		day: 100
		hour: 1
		minute: 2
		second: 3
		offset: 1 hours.
	dateAndTime2 := dateAndTime1 offset: 1 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self assert: dateAndTime1 localSeconds equals: dateAndTime2 localSeconds.

	dateAndTime2 := dateAndTime1 offset: -1 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self deny: dateAndTime1 localSeconds identicalTo: dateAndTime2 localSeconds.

	dateAndTime2 := dateAndTime1 offset: -2 hour.
	self assert: dateAndTime1 equals: dateAndTime2.
	self deny: dateAndTime1 localSeconds identicalTo: dateAndTime2 localSeconds
]

{ #category : #'tests - epoch' }
DateAndTimeTest >> testPharoEpoch [
	self
		useNonUtcTimeZoneDuring: [ | localEpoch |
			localEpoch := '1901-01-01T00:00:00' asDateAndTime.
			self deny: DateAndTime epoch equals: localEpoch.
			self deny: ((DateAndTime fromSeconds: 0) offset: 0) equals: localEpoch ].

	self
		useTimeZone: 'UTC'
		during: [ | localEpoch |
			localEpoch := '1901-01-01T00:00:00' asDateAndTime.
			self assert: DateAndTime epoch equals: localEpoch.
			self assert: ((DateAndTime fromSeconds: 0) offset: 0) equals: localEpoch ].

	self assert: DateAndTime epoch equals: '1901-01-01T00:00:00+00:00' asDateAndTime.
	self assert: ((DateAndTime fromSeconds: 0) offset: 0) equals: '1901-01-01T00:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeTest >> testPrintString [
	| dt dtNoOffset |
	dt := DateAndTime
		year: 2004
		month: 11
		day: 2
		hour: 14
		minute: 3
		second: 5
		nanoSecond: 12345
		offset: (Duration seconds: 5 * 3600).
	self assert: dt printString equals: '2004-11-02T14:03:05.000012345+05:00'.
	self assert: '2002-05-16T17:20:45.1+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.1+01:01'.
	self assert: ' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.02+01:01'.
	self assert: '2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.000000009+01:01'.
	self assert: '2002-05-16T17:20:45+00:00' asDateAndTime printString equals: '2002-05-16T17:20:45+00:00'.
	self assert: ' 2002-05-16T17:20:45+01:57' asDateAndTime printString equals: '2002-05-16T17:20:45+01:57'.
	self assert: ' 2002-05-16T17:20:45-02:34' asDateAndTime printString equals: '2002-05-16T17:20:45-02:34'.
	self assert: '2002-05-16T17:20:45+00:00' asDateAndTime printString equals: '2002-05-16T17:20:45+00:00'.
	self assert: '1997-04-26T01:02:03+01:02:3' asDateAndTime printString equals: '1997-04-26T01:02:03+01:02:3'.
	"When no offset is provided, the local one is used"
	dtNoOffset := '2002-05-16T17:20' asDateAndTime.
	self assert: (('2002-05-16T17:20:00*' match: dtNoOffset printString) and: [ dtNoOffset offset = DateAndTime localOffset ])
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testPrintStringNoOffset [
	| localOffsetHours localOffsetMinutes signString |
	signString := DateAndTime localOffset hours positive ifTrue: [ '+' ] ifFalse: [ '-' ].
	localOffsetHours := DateAndTime localOffset hours abs printStringPadded: 2.
	localOffsetMinutes := DateAndTime localOffset minutes printStringPadded: 2.
	self
		assert: '2002-05-16T17:20' asDateAndTime printString
		equals: ('2002-05-16T17:20:00{1}{2}:{3}' format: {signString . localOffsetHours . localOffsetMinutes}).
	self
		assert: '2002-05-16T17:20:45' asDateAndTime printString
		equals: ('2002-05-16T17:20:45{1}{2}:{3}' format: {signString . localOffsetHours . localOffsetMinutes})
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testPrintStringSecond [	

	self assert: '-1199-01-05T20:33:14.321-05:00' asDateAndTime printString equals: '-1199-01-05T20:33:14.321-05:00'.
	self assert: '2002-05-16T17:20:45.1+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.1+01:01'.
	self assert: '2002-05-16T17:20:45.02+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.02+01:01'.
	self assert: '2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.000000009+01:01'.
	self assert: ' 2002-05-16T17:20:45+01:57' asDateAndTime printString equals: '2002-05-16T17:20:45+01:57'.
	self assert: ' 2002-05-16T17:20:45-02:34' asDateAndTime printString equals: '2002-05-16T17:20:45-02:34'.
	self assert: '2002-05-16T17:20:45+00:00' asDateAndTime printString equals: '2002-05-16T17:20:45+00:00'.
	self assert: '1997-04-26T01:02:03+01:02:3' asDateAndTime printString equals: '1997-04-26T01:02:03+01:02:3'
]

{ #category : #tests }
DateAndTimeTest >> testReadFrom [

	self 
		assert: '-1199-01-05T20:33:14.321-05:00' asDateAndTime printString
		equals: '-1199-01-05T20:33:14.321-05:00'.
	self 
		assert: '2002-05-16T17:20:45.1+01:01' asDateAndTime printString
		equals:  '2002-05-16T17:20:45.1+01:01'.
	self 
		assert:	' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString
		equals:  '2002-05-16T17:20:45.02+01:01'.  
	self 
		assert:	'2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString
		equals:   '2002-05-16T17:20:45.000000009+01:01'.
	self 
		assert: ' 2002-05-16T17:20' asDateAndTime translateToUTC printString
		equals:  '2002-05-16T17:20:00+00:00'.
	self 
		assert: '2002-05-16T17:20:45' asDateAndTime translateToUTC printString
		equals:  '2002-05-16T17:20:45+00:00' .
	self 
		assert: ' 2002-05-16T17:20:45+01:57' asDateAndTime printString
		equals:  '2002-05-16T17:20:45+01:57'.
	self 
		assert: ' 2002-05-16T17:20:45-02:34' asDateAndTime
		equals:  '2002-05-16T17:20:45-02:34' asDateAndTime.
	self 
		assert: '2002-05-16T17:20:45+00:00' asDateAndTime
		equals:  '2002-05-16T17:20:45+00:00' asDateAndTime.
	self 
		assert: '1997-04-26T01:02:03+01:02:3' asDateAndTime
		equals:  '1997-04-26T01:02:03+01:02:3' asDateAndTime
]

{ #category : #tests }
DateAndTimeTest >> testReadFromDateOnly [
	self 
		assert: '2014-02-19' asDateAndTime translateToUTC printString
		equals: '2014-02-19T00:00:00+00:00'.
	self 
		assert: '2014-02-19Z' asDateAndTime printString
		equals: '2014-02-19T00:00:00+00:00'.
	self 
		assert: '2014-02-19T+07:00' asDateAndTime printString
		equals:  '2014-02-19T00:00:00+07:00'.
	self 
		assert: '2014-02-19 -05:00' asDateAndTime printString
		equals:  '2014-02-19T00:00:00-05:00'.

]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadFromDefaultOffsetNotSpecified [

	self assert: (DateAndTime readFrom: '2016-07-03T21:16:16.708241' readStream defaultOffset: Duration zero) offset equals: Duration zero.

]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadFromDefaultOffsetSpecified [

	self assert: (DateAndTime readFrom: '2016-07-03T21:16:16.708241+02:00' readStream defaultOffset: Duration zero) offset equals: (Duration hours: 2).

]

{ #category : #tests }
DateAndTimeTest >> testReadFromFoolProofExtension [
	"Convenient extension without a time, only a date"

	self assert: (DateAndTime fuzzyReadFrom: '2008' readStream) printString equals: '2008-01-01T00:00:00+00:00'.
	self assert: (DateAndTime fuzzyReadFrom: '2008-08' readStream) printString equals: '2008-08-01T00:00:00+00:00'.
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28' readStream) printString equals: '2006-08-28T00:00:00+00:00'.
	"Regular nanoseconds"
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28T00:00:00.123456789' readStream) printString equals: '2006-08-28T00:00:00.123456789+00:00'.
	"Extra picoseconds precision should not spoil the DateAndTime"
	self assert: (DateAndTime fuzzyReadFrom: '2006-08-28T00:00:00.123456789000' readStream) printString equals: '2006-08-28T00:00:00.123456789+00:00'
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testReadFromNoOffset [
	
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321' readStream) offset equals: DateAndTime localOffset
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadFromOffset [
		
	self assert: (DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream) offset printString equals: '-0:05:00:00'.

]

{ #category : #'tests - under design' }
DateAndTimeTest >> testReadFromSecond [
	self assert: '-1199-01-05T20:33:14.321-05:00' asDateAndTime printString equals: '-1199-01-05T20:33:14.321-05:00'.
	self assert: '2002-05-16T17:20:45.1+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.1+01:01'.
	self assert: ' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.02+01:01'.
	self assert: '2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString equals: '2002-05-16T17:20:45.000000009+01:01'.
	self assert: ' 2002-05-16T17:20' asDateAndTime translateToUTC printString equals: '2002-05-16T17:20:00+00:00'.
	self assert: '2002-05-16T17:20:45' asDateAndTime translateToUTC printString equals: '2002-05-16T17:20:45+00:00'.
	self assert: ' 2002-05-16T17:20:45+01:57' asDateAndTime printString equals: '2002-05-16T17:20:45+01:57'.
	self assert: ' 2002-05-16T17:20:45-02:34' asDateAndTime printString equals: '2002-05-16T17:20:45-02:34'.
	self assert: '2002-05-16T17:20:45+00:00' asDateAndTime printString equals: '2002-05-16T17:20:45+00:00'.
	self assert: '1997-04-26T01:02:03+01:02:3' asDateAndTime printString equals: '1997-04-26T01:02:03+01:02:3'
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadFromSpaceBeforeOffset [
	self assert: '2012-07-26 16:38:48 +0200' asDateAndTime offset equals: 2 hour.
	self assert: '2012-07-26 16:38:48 +02' asDateAndTime offset equals: 2 hour.


]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadFromUTCOffset [	
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321Z' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+00' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+0000' readStream) offset isZero.
	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321+00:00' readStream) offset isZero.


]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadTimeWithOtherCharactersAfter [
	"See https://pharo.manuscript.com/f/cases/17693/"
	self
		assert:
			(DateAndTime
				readFrom:
					'2016-02-03 19:23:48 +0131 By: Esteban Lorenzano <estebanlm@gmail.com> Jenkins build #547'
						readStream) printString
		equals: '2016-02-03T19:23:48+01:31'
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadTimeZoneOffsetISO [
	"See https://pharo.manuscript.com/f/cases/17693 
	 See ISO 8601 offset format"

	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+05:30' readStream) printString
		equals: '0:05:30:00'
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadTimeZoneOffsetISO822WithOtherCharactersAfter [
	"See https://pharo.manuscript.com/f/cases/17693/"

	self assert: (DateAndTime readTimezoneOffsetFrom: '+05:30 CommiterName' readStream) printString equals: '0:05:30:00'
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadTimeZoneOffsetRFC822 [
	"See https://pharo.manuscript.com/f/cases/17693
	 See RFC822 : https://www.w3.org/Protocols/rfc822"

	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+0530' readStream) printString
		equals: '0:05:30:00'
]

{ #category : #'tests - offset' }
DateAndTimeTest >> testReadTimeZoneOffsetRFC822WithOtherCharactersAfter [
	"See https://pharo.manuscript.com/f/cases/17693"
	
	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+0530 CommiterName' readStream) printString
		equals: '0:05:30:00'
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testReflexive [

	| t |
	t := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	self assert: t equals: t.
	

]

{ #category : #'tests - epoch' }
DateAndTimeTest >> testSecondsAcrossTimeZones [

	| dateTime seconds dateTime2 utc |
	dateTime := '1/15/2012 0000+00:00' asDateAndTime.
	utc := TimeZone abbreviated: 'UTC'.

	self useTimeZone: 'PDT' during: [ :pdt |
		"Store a DateAndTime as seconds from the epoch"
		seconds := dateTime asSeconds.

		"Now move to Greenwich" 
		DateAndTime localTimeZone: utc. 
		dateTime2 := DateAndTime fromSeconds: seconds.
		
		self assert: dateTime equals: dateTime2 ].
]

{ #category : #tests }
DateAndTimeTest >> testSecondsRoundTrip [
	| now now2 |
	now := DateAndTime fromSeconds: 0.
	now2 := DateAndTime fromSeconds: now asSeconds.
	self assert: now equals: now2.
]

{ #category : #tests }
DateAndTimeTest >> testSecondsSinceMidnightLocalTime [
	| dateAndTime |
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 1 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 0 second: 56 offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.
	
	"1 hour offset"
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 1 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 1 minute: 0 second: 56 offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.
	
	"-1 hour offset"
	dateAndTime := DateAndTime year: 1000 day: 100 hour: 0 minute: 0 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 56.
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 60+56.
	
	dateAndTime := DateAndTime year: 1002 day: 102 hour: 1 minute: 0 second: 56 offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 3600+56.
]

{ #category : #tests }
DateAndTimeTest >> testSecondsSinceMidnightLocalTimeNormalization [
	| dateAndTime|
	
	"Check offset: changes from times defined in UTC"
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: 0 hours.
	dateAndTime := dateAndTime offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (1*3600) + (1*60) + 56.
	"-1 hours will switch to the previous day"
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (23*3600) + (1*60) + 56.
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 23 minute: 1 second: 56 offset: 0 hours.
	dateAndTime := dateAndTime offset: 1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (0*3600) + (1*60) + 56.
	"-1 hours will switch to the next day"
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: 22*3600 + (1*60) + 56.	
		
		
	
	"Check offset: changes from times defined in +1"
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 0 minute: 1 second: 56 offset: 1 hours.
	dateAndTime := dateAndTime offset: 2 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (1*3600) + (1*60) + 56.
	"0 hours will switch to the previous day"
	dateAndTime := dateAndTime offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (23*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (22*3600) + (1*60) + 56.
	
	
	dateAndTime := DateAndTime year: 1001 day: 101 hour: 23 minute: 1 second: 56 offset: 1 hours.
	"+2 hours will switch to the next day"
	dateAndTime := dateAndTime offset: 2 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (0*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: 0 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (22*3600) + (1*60) + 56.
	dateAndTime := dateAndTime offset: -1 hours.
	self assert: dateAndTime secondsSinceMidnightLocalTime equals: (21*3600) + (1*60) + 56.

]

{ #category : #'tests - instance' }
DateAndTimeTest >> testSimpleAccessors [
	| t |
	t := DateAndTime
		year: 1
		month: 1
		day: 2
		hour: 2
		minute: 3
		second: 4
		nanoSecond: 5
		offset: 6 hours.
	self
		assert: t hours equals: t hours;
		assert: t minutes equals: t minute;
		assert: t seconds equals: t second
]

{ #category : #'tests - arithmetic' }
DateAndTimeTest >> testSubstracting [

	| t |
	t := '2004-01-07T11:55:00+00:00' asDateAndTime. 
	self assert: t - 5 equals: '2004-01-07T11:54:55+00:00' asDateAndTime

]

{ #category : #'tests - arithmetic' }
DateAndTimeTest >> testSubstracting02 [

	| t |
	t := '2004-01-07T11:55:00+00:00' asDateAndTime. 
	self assert: t - '2004-01-07T1:00:00+00:00' equals: (Duration fromString: '0:10:55:00')
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testSymmetric [
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
]

{ #category : #tests }
DateAndTimeTest >> testTimeZoneEquivalence [
	"DateAndTimeTest new testTimeZoneEquivalence"

	"When the clock on the wall in Detroit says 9:00am, the clock on the wall
	in London says 2:00pm. The Duration difference between the corresponding
	DateAndTime values should be zero."

	" Detroit is 5 hours behind UTC, this offset to UTC is therefore written with a minus sign. This example tests both the correct interpretation of the DateAndTime denotation and correct DateAndTime arithmetics. "

	| twoPmInLondon nineAmInDetroit durationDifference |
	twoPmInLondon := '2004-11-02T14:00:00+00:00' asDateAndTime.
	nineAmInDetroit := '2004-11-02T09:00:00-05:00' asDateAndTime.
	durationDifference := twoPmInLondon - nineAmInDetroit.
	self assert: durationDifference asSeconds equals: 0.
	self assert: twoPmInLondon equals: nineAmInDetroit
]

{ #category : #tests }
DateAndTimeTest >> testTimeZoneEquivalence2 [
	"DateAndTimeTest new testTimeZoneEquivalence2"

	"This example demonstates the fact that
        2004-05-24T22:40:00  UTC  is
        2004-05-25T01:40:00  in Moscow
     (Moscow is 3 hours ahead of UTC)  "

	| thisMoment thisMomentInMoscow |
	thisMoment := DateAndTime
		year: 2004
		month: 5
		day: 24
		hour: 22
		minute: 40.
	thisMomentInMoscow := thisMoment offset: 3 hours.
	self assert: (thisMoment - thisMomentInMoscow) asSeconds equals: 0.
	self assert: thisMoment equals: thisMomentInMoscow
]

{ #category : #'tests - under design' }
DateAndTimeTest >> testTransitive [	
	| t1 t2 t3 |
	t1 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	t2 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	t3 := DateAndTime 
			year: 1 month: 1 day: 2 
			hour: 2 minute: 3 second: 4 nanoSecond: 5 
			offset: 6 hours.
	self assert: ((t1 = t2) & (t2 = t3) ==> (t1 = t3)).
	
	

]

{ #category : #'tests - epoch' }
DateAndTimeTest >> testUnixEpoch [

	self assert: DateAndTime unixEpoch equals: '1 January 1970 00:00+00:00' asDateAndTime.	
]

{ #category : #tests }
DateAndTimeTest >> testYearMonth [
	
	self assert: ((DateAndTime year: 2018 month: 9) asDate asString) equals: '1 September 2018'.
]

{ #category : #helpers }
DateAndTimeTest >> useNonUtcTimeZoneDuring: aBlock [

	self useTimeZone: 'EDT' during: aBlock.
]

{ #category : #helpers }
DateAndTimeTest >> useTimeZone: abbreviation during: aBlock [

  | timeZone |
  timeZone := TimeZone abbreviated: abbreviation.
  self restoreLocalTimeZoneAfter: [ 
    DateAndTime localTimeZone: timeZone. 
    aBlock cull: timeZone ].
]
