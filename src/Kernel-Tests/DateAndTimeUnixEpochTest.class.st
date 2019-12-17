"
Tests for unix epoch of DateAndTime
"
Class {
	#name : #DateAndTimeUnixEpochTest,
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
DateAndTimeUnixEpochTest >> setUp [
	super setUp.
   localTimeZoneToRestore := DateAndTime localTimeZone.
	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; unixEpoch.
	aTimeZone := TimeZone offset: (Duration minutes: 135) name: 'Unix Epoch Test Time Zone' abbreviation: 'UTZ'.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 
]

{ #category : #running }
DateAndTimeUnixEpochTest >> tearDown [
     DateAndTime localTimeZone: localTimeZoneToRestore.
     super tearDown

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsDate [
	self assert: aDateAndTime asDate equals: 'January 1, 1970' asDate
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsDateAndTime [
	self assert: aDateAndTime asDateAndTime equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsDuration [
	self assert: aDateAndTime asDuration equals: 0 asDuration
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsLocal [
	self assert: aDateAndTime asLocal equals: aDateAndTime.
	self assert: aDateAndTime asLocal equals: (aDateAndTime offset: aDateAndTime class localOffset)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds equals: 0 asDuration asNanoSeconds
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsSeconds [
	self assert: aDateAndTime asSeconds equals: 2177452800
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsTime [
	self assert: aDateAndTime asTime equals: Time midnight
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsUTC [
	self assert: aDateAndTime asUTC equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testCurrent [
	self deny: aDateAndTime equals: DateAndTime current
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDateTime [
	self assert: aDateAndTime equals: (DateAndTime date: '01-01-1970' asDate time: '00:00:00' asTime)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDay [
	self assert: aDateAndTime day equals: DateAndTime new day
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayMonthYearDo [
	| iterations |
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | iterations := iterations + 1 ]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachYear ]) equals: 1970.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachMonth ]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [ :eachDay :eachMonth :eachYear | eachDay ]) equals: 1
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth equals: 1
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek equals: 5.
	self assert: aDateAndTime dayOfWeekAbbreviation equals: 'Thu'.
	self assert: aDateAndTime dayOfWeekName equals: 'Thursday'
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear equals: 1
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth equals: 31
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear equals: 365
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear equals: 364
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDuration [
	self assert: aDateAndTime duration equals: 0 asDuration
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testEpoch [
	self assert: aDateAndTime equals: '1970-01-01T00:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFirstDayOfMonth [
	self assert: aDateAndTime firstDayOfMonth equals: 1
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFromSeconds [
	self assert: aDateAndTime equals: (DateAndTime fromSeconds: 2177452800)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFromString [
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1970-01-01T00:00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1970-01-01T00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00')
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHash [
	self assert: aDateAndTime hash equals: (DateAndTime year: 1970 month: 1 day: 1) hash
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHour [
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 0.
	self assert: aDateAndTime hour equals: aDateAndTime hours
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHour12 [
	self assert: aDateAndTime hour12 equals: DateAndTime new hour12.
	self assert: aDateAndTime hour12 equals: 12
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testIsLeapYear [
	self deny: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testJulianDayNumber [
	self assert: aDateAndTime equals: (DateAndTime julianDayNumber: 2440588).
	self assert: aDateAndTime julianDayNumber equals: 2440588
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation equals: 'AM'
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) equals: (Timespan starting: '12-31-1969' asDate duration: 2 days)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMidnight [
	self assert: aDateAndTime midnight equals: aDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMinus [
	self assert: aDateAndTime - aDateAndTime equals: '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration equals: aDateAndTime.
	self
		assert: aDateAndTime - aDuration
		equals:
			(DateAndTime
				year: 1969
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
DateAndTimeUnixEpochTest >> testMinute [
	self assert: aDateAndTime minute equals: 0
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMinutes [
	self assert: aDateAndTime minutes equals: 0
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMonth [
	self assert: aDateAndTime month equals: 1.
	self assert: aDateAndTime monthAbbreviation equals: 'Jan'.
	self assert: aDateAndTime monthName equals: 'January'.
	self assert: aDateAndTime monthIndex equals: 1
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond equals: 0
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNoon [
	self assert: aDateAndTime noon equals: '1970-01-01T12:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNow [
	self deny: aDateAndTime equals: DateAndTime now
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testOffset [
	self assert: aDateAndTime offset equals: '0:00:00:00' asDuration.
	self assert: (aDateAndTime offset: '0:12:00:00') equals: '1970-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testPlus [
	self assert: aDateAndTime + '0:00:00:00' equals: aDateAndTime.
	self assert: aDateAndTime + 0 equals: aDateAndTime.
	self
		assert: aDateAndTime + aDuration
		equals:
			(DateAndTime
				year: 1970
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
DateAndTimeUnixEpochTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1970-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(UTZ)'
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testSecond [
	self assert: aDateAndTime second equals: 0
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testSeconds [
	self assert: aDateAndTime seconds equals: 0
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTicks [
	self assert: aDateAndTime ticks equals: (DateAndTime julianDayNumber: 2440588) ticks.
	self assert: aDateAndTime ticks equals: #(2440588 0 0)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTicksOffset [
	self assert: aDateAndTime equals: (aDateAndTime ticks: #(2415386 0 0) offset: DateAndTime localOffset)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTimeZone [
	self assert: aDateAndTime timeZoneName equals: 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation equals: 'UTC'
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTo [
	self assert: (aDateAndTime to: aDateAndTime) equals: ((DateAndTime year: 1970 month: 1 day: 1) to: (DateAndTime year: 1970 month: 1 day: 1))
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToBy [
	self
		assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days)
		equals: ((DateAndTime year: 1970 month: 1 day: 1) to: (DateAndTime year: 1970 month: 1 day: 1) + 10 days by: 5 days)
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToByDo [
	"self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days do: []) =  "
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToday [
	self deny: aDateAndTime equals: DateAndTime today
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTommorrow [
	self assert: DateAndTime today + 24 hours equals: DateAndTime tomorrow.
	self deny: aDateAndTime equals: DateAndTime tomorrow
	"MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testUtcOffset [
	self assert: (aDateAndTime offset: '0:12:00:00') equals: '1970-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYear [
	self assert: aDateAndTime year equals: 1970
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1970 day: 1)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearDayHourMinuteSecond [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1970
				day: 1
				hour: 0
				minute: 0
				second: 0)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1970 month: 1 day: 1)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDayHourMinuteSecond [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1970
				month: 1
				day: 1
				hour: 0
				minute: 0
				second: 0)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDayHourMinuteSecondNanosSecondOffset [
	self
		assert: aDateAndTime
		equals:
			(DateAndTime
				year: 1970
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
DateAndTimeUnixEpochTest >> testYesterday [
	self deny: aDateAndTime equals: DateAndTime yesterday
]
