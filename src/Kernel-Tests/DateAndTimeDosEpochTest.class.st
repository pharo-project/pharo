"
SUnit test for dos epoch (see DateAndTime)
"
Class {
	#name : #DateAndTimeDosEpochTest,
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
DateAndTimeDosEpochTest >> setUp [

	super setUp.
	localTimeZoneToRestore := DateAndTime localTimeZone.
	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; dosEpoch.
	aTimeZone := TimeZone offset: (Duration minutes: 135) name: 'DOS Epoch Test Time Zone' abbreviation: 'DTZ'.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 
]

{ #category : #running }
DateAndTimeDosEpochTest >> tearDown [
     "wish I could remove the time zones I added earlier, but there is no method for that"

     DateAndTime localTimeZone: localTimeZoneToRestore.
	  super tearDown

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsDate [
	self assert: aDateAndTime asDate equals: 'January 1, 1980' asDate


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsDateAndTime [
	self assert: aDateAndTime asDateAndTime equals: aDateAndTime
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsDuration [
	self assert: aDateAndTime asDuration equals: 0 asDuration
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsLocal [
	self assert: aDateAndTime asLocal equals: aDateAndTime.
	self assert: aDateAndTime asLocal equals: (aDateAndTime offset: aDateAndTime class localOffset)
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds equals: 0 asDuration asNanoSeconds
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsSeconds [
	self assert: aDateAndTime asSeconds equals: 2492985600
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsTime [
	self assert: aDateAndTime asTime equals: Time midnight

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsUTC [
	self assert: aDateAndTime asUTC equals: aDateAndTime
          
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testCurrent [
	self deny: aDateAndTime equals: (DateAndTime current)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDateTime [
	self assert: aDateAndTime equals: (DateAndTime date: '01-01-1980' asDate time: '00:00:00' asTime)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDay [
	self assert: aDateAndTime day equals: DateAndTime new day

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayMonthYearDo [
	|iterations|
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  iterations := iterations + 1]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear]) equals: 1980.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) equals: 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) equals: 1

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth equals: 1

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek equals: 3.
	self assert: aDateAndTime dayOfWeekAbbreviation equals: 'Tue'.
	self assert: aDateAndTime dayOfWeekName equals: 'Tuesday'

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear equals: 1


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth equals: 31


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear equals: 366


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear equals: 365


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDuration [
	self assert: aDateAndTime duration equals: 0 asDuration


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testEpoch [
	self assert: aDateAndTime equals: '1980-01-01T00:00:00+00:00' asDateAndTime

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFirstDayOfMonth [
	self assert: aDateAndTime firstDayOfMonth equals: 1

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFromSeconds [
	self assert: aDateAndTime equals: (DateAndTime fromSeconds: 2492985600)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFromString [
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1980-01-01T00:00:00+00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1980-01-01T00:00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1980-01-01T00:00').
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 1980-01-01T00:00:00+00:00')

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHash [
	self assert: aDateAndTime hash equals: (DateAndTime year: 1980 month: 1 day: 1) hash
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHour [
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 0.
	self assert: aDateAndTime hour equals: aDateAndTime hours

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHour12 [
	self assert: aDateAndTime hour12 equals: DateAndTime new hour12.
	self assert: aDateAndTime hour12 equals: 12

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testIsLeapYear [
	self assert: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testJulianDayNumber [
	self assert: aDateAndTime equals: (DateAndTime julianDayNumber: 2444240).
	self assert: aDateAndTime julianDayNumber equals: 2444240
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime
	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation equals: 'AM'

	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) 
	     equals: (Timespan starting: '12-31-1979' asDate duration: 2 days)
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMidnight [
	self assert: aDateAndTime midnight equals: aDateAndTime

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinus [
	self assert: aDateAndTime - aDateAndTime equals: '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration equals: aDateAndTime.
	self assert: aDateAndTime - aDuration equals: (DateAndTime year: 1979 month: 12 day: 30 hour: 21 minute: 56 second: 55 nanoSecond: 999999995 offset: 0 hours)
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinute [
	self assert: aDateAndTime minute equals: 0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinutes [
	self assert: aDateAndTime minutes equals: 0

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMonth [
	self assert: aDateAndTime month equals: 1.
	self assert: aDateAndTime monthAbbreviation equals: 'Jan'.
	self assert: aDateAndTime monthName equals: 'January'.
	self assert: aDateAndTime monthIndex equals: 1
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond equals: 0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNoon [
	self assert: aDateAndTime noon equals: '1980-01-01T12:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNow [
	self deny: aDateAndTime equals: (DateAndTime now)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testOffset [

	self assert: aDateAndTime offset equals: '0:00:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals: '1979-12-31T12:00:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals: '1980-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testPlus [
	self assert: aDateAndTime + '0:00:00:00' equals: aDateAndTime.
	self assert: aDateAndTime + 0 equals: aDateAndTime.
	self assert: aDateAndTime + aDuration equals: (DateAndTime year: 1980 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1980-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(DTZ)'
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testSecond [
	self assert: aDateAndTime second equals: 0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testSeconds [
	self assert: aDateAndTime seconds equals: 0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTicks [
	self assert: aDateAndTime ticks equals: (DateAndTime julianDayNumber: 2444240) ticks.
	self assert: aDateAndTime ticks equals: #(2444240 0 0)
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTicksOffset [
	self assert: aDateAndTime equals: (aDateAndTime ticks:  #(2444240 0 0) offset: DateAndTime localOffset)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTimeZone [
	self assert: aDateAndTime timeZoneName equals: 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation	 equals: 'UTC'


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTo [
	self assert: (aDateAndTime to: aDateAndTime) equals: ((DateAndTime year: 1980 month: 1 day: 1) to: (DateAndTime year: 1980 month: 1 day: 1))
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToBy [
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) equals: 
				((DateAndTime year: 1980 month: 1 day: 1) to:
				 (DateAndTime year: 1980 month: 1 day: 1) + 10 days by: 5 days)
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToByDo [
	| schedule |
	schedule := aDateAndTime to: aDateAndTime + 10 days by: 5 days do: [:i | ].
	self 
		assert: schedule notNil;
		assert: schedule duration equals: 10 days
	 
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToday [
	self deny: aDateAndTime equals: DateAndTime today
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTommorrow [
	self assert: DateAndTime today + 24 hours equals: DateAndTime tomorrow.
	self deny: aDateAndTime equals: DateAndTime tomorrow
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testUtcOffset [
     self assert: (aDateAndTime offset: '0:12:00:00')  equals: '1980-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYear [
	self assert: aDateAndTime year equals: 1980

	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1980 day: 1)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearDayHourMinuteSecond [
	self assert: aDateAndTime equals: (DateAndTime year: 1980 day: 1 hour: 0 minute: 0 second: 0)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDay [
	self assert: aDateAndTime equals: (DateAndTime year: 1980 month: 1 day: 1)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDayHourMinuteSecond [
	self assert: aDateAndTime equals: (DateAndTime year: 1980 month: 1 day: 1 hour: 0 minute: 0 second: 0)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDayHourMinuteSecondNanosSecondOffset [
	self assert: aDateAndTime equals: (DateAndTime year: 1980 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset:0 hours ).
	self assert: ((DateAndTime year: 1 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset: 0 hours ) +
				(Duration days: 1 hours: 2 minutes: 3 seconds: 4  nanoSeconds: 5) ) equals:  	
				(DateAndTime year: 1 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours) 
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYesterday [
	self deny: aDateAndTime equals: DateAndTime yesterday
]
