"
I represent one of several Sunit test Cases intentended to provide complete coverage  for the Chronology set of classes as part of the external testing. The other Chronology sunit test cases are:
 DateTestCase
 DateAndTimeLeapTestCase,
 DurationTestCase,
 ScheduleTestCase
 TimeStampTestCase
 TimespanDoTestCase, 
 TimespanDoSpanAYearTestCase, 
 TimespanTestCase, 
 YearMonthWeekTestCase.  
These tests attempt to exercise all public and private methods.  Except, they do not explicitly depreciated methods. tlk
My fixtures are:
aDateAndTime = January 01, 1901 midnight (the start of the Squeak epoch) with localTimeZone = Grenwhich Meridian (local offset = 0 hours)
aDuration = 1 day, 2 hours, 3, minutes, 4 seconds and 5 nano seconds.
aTimeZone =  'Epoch Test Time Zone', 'ETZ' , offset: 12 hours, 15 minutes. 
"
Class {
	#name : #DateAndTimeEpochTest,
	#superclass : #TestCase,
	#instVars : [
		'aDateAndTime',
		'aDuration',
		'aTimeZone',
		'localTimeZoneToRestore'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #running }
DateAndTimeEpochTest >> setUp [
 	super setUp.  
	localTimeZoneToRestore := DateAndTime localTimeZone.
	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; epoch.
	aTimeZone := TimeZone offset: (Duration minutes: 135) name: 'Epoch Test Time Zone' abbreviation: 'ETZ'.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 
]

{ #category : #running }
DateAndTimeEpochTest >> tearDown [
     DateAndTime localTimeZone: localTimeZoneToRestore.
     super tearDown

]

{ #category : #tests }
DateAndTimeEpochTest >> testAsDate [
	self assert: aDateAndTime asDate equals: 'January 1, 1901' asDate
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsDateAndTime [
	self assert: aDateAndTime asDateAndTime equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsDuration [
	self assert: aDateAndTime asDuration equals: 0 asDuration
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsLocal [
	self assert: aDateAndTime asLocal equals: aDateAndTime.
	self assert: aDateAndTime asLocal equals: (aDateAndTime offset: aDateAndTime class localOffset)
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds equals: 0 asDuration asNanoSeconds
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsSeconds [
	self assert: aDateAndTime asSeconds equals: 0 asDuration asSeconds
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsTime [
	self assert: aDateAndTime asTime equals: Time midnight
]

{ #category : #tests }
DateAndTimeEpochTest >> testAsUTC [
	self assert: aDateAndTime asUTC equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testCurrent [
	self deny: aDateAndTime equals: DateAndTime current
]

{ #category : #tests }
DateAndTimeEpochTest >> testDateTime [
	self assert: aDateAndTime equals: (DateAndTime date: '01-01-1901' asDate time: '00:00:00' asTime)
]

{ #category : #tests }
DateAndTimeEpochTest >> testDay [
	self assert: aDateAndTime day equals: DateAndTime new day
]

{ #category : #tests }
DateAndTimeEpochTest >> testDayMonthYearDo [
	| iterations |
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | iterations := iterations + 1 ]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachYear ]) equals: 1901.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachMonth ]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachDay ]) equals: 1
]

{ #category : #tests }
DateAndTimeEpochTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth equals: 1
]

{ #category : #tests }
DateAndTimeEpochTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek equals: 3.
	self assert: aDateAndTime dayOfWeekAbbreviation equals: 'Tue'.
	self assert: aDateAndTime dayOfWeekName equals: 'Tuesday'
]

{ #category : #tests }
DateAndTimeEpochTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear equals: 1
]

{ #category : #tests }
DateAndTimeEpochTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth equals: 31
]

{ #category : #tests }
DateAndTimeEpochTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear equals: 365
]

{ #category : #tests }
DateAndTimeEpochTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear equals: 364
]

{ #category : #tests }
DateAndTimeEpochTest >> testDuration [
	self assert: aDateAndTime duration equals: 0 asDuration
]

{ #category : #tests }
DateAndTimeEpochTest >> testEpoch [
	self assert: aDateAndTime equals: '1901-01-01T00:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testFirstDayOfMonth [
	self assert: aDateAndTime firstDayOfMonth equals: 1
]

{ #category : #tests }
DateAndTimeEpochTest >> testFromSeconds [
	self assert: aDateAndTime equals: (DateAndTime fromSeconds: 0)
]

{ #category : #tests }
DateAndTimeEpochTest >> testFromString [
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1901-01-01T00:00:00+00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1901-01-01T00:00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1901-01-01T00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1901-01-01T00:00:00+00:00')
]

{ #category : #tests }
DateAndTimeEpochTest >> testHash [
	self assert: aDateAndTime hash equals: DateAndTime new hash
]

{ #category : #tests }
DateAndTimeEpochTest >> testHour [
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 0.
	self assert: aDateAndTime hour equals: aDateAndTime hours
]

{ #category : #tests }
DateAndTimeEpochTest >> testHour12 [
	self assert: aDateAndTime hour12 equals: DateAndTime new hour12.
	self assert: aDateAndTime hour12 equals: 12
]

{ #category : #tests }
DateAndTimeEpochTest >> testIsLeapYear [
	self deny: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeEpochTest >> testJulianDayNumber [
	self assert: aDateAndTime equals: (DateAndTime julianDayNumber: 2415386).
	self assert: aDateAndTime julianDayNumber equals: 2415386
]

{ #category : #tests }
DateAndTimeEpochTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	
]

{ #category : #tests }
DateAndTimeEpochTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation equals: 'AM'
]

{ #category : #tests }
DateAndTimeEpochTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) equals: (Timespan starting: '12-31-1900' asDate duration: 2 days)
]

{ #category : #tests }
DateAndTimeEpochTest >> testMidnight [
	self assert: aDateAndTime midnight equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testMinus [
	self assert: aDateAndTime - aDateAndTime equals: '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration equals: aDateAndTime.
	self
		assert: aDateAndTime - aDuration
		equals:
			(DateAndTime
				year: 1900
				month: 12
				day: 30
				hour: 21
				minute: 56
				second: 55
				nanoSecond: 999999995
				offset: 0 hours)
	" I believe this Failure is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
]

{ #category : #tests }
DateAndTimeEpochTest >> testMinute [
	self assert: aDateAndTime minute equals: 0
]

{ #category : #tests }
DateAndTimeEpochTest >> testMinutes [
	self assert: aDateAndTime minutes equals: 0
]

{ #category : #tests }
DateAndTimeEpochTest >> testMonth [
	self assert: aDateAndTime month equals: 1.
	self assert: aDateAndTime monthAbbreviation equals: 'Jan'.
	self assert: aDateAndTime monthName equals: 'January'.
	self assert: aDateAndTime monthIndex equals: 1
]

{ #category : #tests }
DateAndTimeEpochTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond equals: 0
]

{ #category : #tests }
DateAndTimeEpochTest >> testNew [
	self assert: aDateAndTime equals: DateAndTime new
]

{ #category : #tests }
DateAndTimeEpochTest >> testNoon [
	self assert: aDateAndTime noon equals: '1901-01-01T12:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testNow [
	self deny: aDateAndTime equals: DateAndTime now
]

{ #category : #tests }
DateAndTimeEpochTest >> testOffset [
	self assert: aDateAndTime offset equals: '0:00:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals: '1900-12-31T12:00:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals: '1901-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testPlus [
	self assert: aDateAndTime + '0:00:00:00' equals: aDateAndTime.
	self assert: aDateAndTime + 0 equals: aDateAndTime.
	self
		assert: aDateAndTime + aDuration
		equals:
			(DateAndTime
				year: 1901
				month: 1
				day: 2
				hour: 2
				minute: 3
				second: 4
				nanoSecond: 5
				offset: 0 hours)
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
]

{ #category : #tests }
DateAndTimeEpochTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1901-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(ETZ)'
]

{ #category : #tests }
DateAndTimeEpochTest >> testSecond [
	self assert: aDateAndTime second equals: 0
]

{ #category : #tests }
DateAndTimeEpochTest >> testSeconds [
	self assert: aDateAndTime seconds equals: 0
]

{ #category : #tests }
DateAndTimeEpochTest >> testTicks [
	self assert: aDateAndTime ticks equals: (DateAndTime julianDayNumber: 2415386) ticks.
	self assert: aDateAndTime ticks equals: #(2415386 0 0)
]

{ #category : #tests }
DateAndTimeEpochTest >> testTicksOffset [
	self assert: aDateAndTime equals: (aDateAndTime ticks: #(2415386 0 0) offset: DateAndTime localOffset)
]

{ #category : #tests }
DateAndTimeEpochTest >> testTimeZone [
	self assert: aDateAndTime timeZoneName equals: 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation equals: 'UTC'
]

{ #category : #tests }
DateAndTimeEpochTest >> testTo [
	self assert: (aDateAndTime to: aDateAndTime) equals: (DateAndTime new to: DateAndTime new)
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeEpochTest >> testToBy [
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) equals: (DateAndTime new to: DateAndTime new + 10 days by: 5 days)
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeEpochTest >> testToByDo [
	"self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days do: []) =  "
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeEpochTest >> testToday [
	self deny: aDateAndTime equals: DateAndTime today
]

{ #category : #tests }
DateAndTimeEpochTest >> testTommorrow [
	self assert: DateAndTime today + 24 hours equals: DateAndTime tomorrow.
	self deny: aDateAndTime equals: DateAndTime tomorrow
	"MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
DateAndTimeEpochTest >> testUtcOffset [
	self assert: (aDateAndTime offset: '0:12:00:00') equals: '1901-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeEpochTest >> testYear [
	self assert: aDateAndTime year equals: 1901
]

{ #category : #tests }
DateAndTimeEpochTest >> testYearDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1901 day: 1)
]

{ #category : #tests }
DateAndTimeEpochTest >> testYearDayHourMinuteSecond [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1901
				day: 1
				hour: 0
				minute: 0
				second: 0)
]

{ #category : #tests }
DateAndTimeEpochTest >> testYearMonthDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1901 month: 1 day: 1)
]

{ #category : #tests }
DateAndTimeEpochTest >> testYearMonthDayHourMinuteSecond [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1901
				month: 1
				day: 1
				hour: 0
				minute: 0
				second: 0)
]

{ #category : #tests }
DateAndTimeEpochTest >> testYearMonthDayHourMinuteSecondNanosSecondOffset [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1901
				month: 1
				day: 1
				hour: 0
				minute: 0
				second: 0
				nanoSecond: 0
				offset: 0 hours).
	self
		assert:
			(DateAndTime
				year: 1
				month: 1
				day: 1
				hour: 0
				minute: 0
				second: 0
				nanoSecond: 0
				offset: 0 hours)
				+
					(Duration
						days: 1
						hours: 2
						minutes: 3
						seconds: 4
						nanoSeconds: 5)
		equals:
			(DateAndTime
				year: 1
				month: 1
				day: 2
				hour: 2
				minute: 3
				second: 4
				nanoSecond: 5
				offset: 0 hours)
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
]

{ #category : #tests }
DateAndTimeEpochTest >> testYesterday [
	self deny: aDateAndTime equals: DateAndTime yesterday
]
