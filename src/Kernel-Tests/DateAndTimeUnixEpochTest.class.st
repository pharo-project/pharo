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
	self assert: aDateAndTime asDate =   'January 1, 1970' asDate.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsDateAndTime [
	self assert: aDateAndTime asDateAndTime =  aDateAndTime
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsDuration [
	self assert: aDateAndTime asDuration =  0 asDuration
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsLocal [
	self assert: aDateAndTime asLocal =  aDateAndTime.
	self assert: aDateAndTime asLocal = (aDateAndTime offset: aDateAndTime class localOffset)
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsMonth [
	self 
		assert: aDateAndTime asMonth
		equals: (Month year: 1970 month: 'January'). 

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds =  0 asDuration asNanoSeconds
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsSeconds [
	self assert: aDateAndTime asSeconds = 2177452800
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsTime [
	self assert: aDateAndTime asTime =  Time midnight.

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsUTC [
	self assert: aDateAndTime asUTC =  aDateAndTime
          
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek = (Week starting: '12-31-1969' asDate). 


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testAsYear [
	self assert: aDateAndTime asYear =   (Year starting: '01-01-1970' asDate). 

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testCurrent [
	self deny: aDateAndTime =  (DateAndTime current).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDateTime [
	self assert: aDateAndTime =  (DateAndTime date: '01-01-1970' asDate time: '00:00:00' asTime)

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDay [
	self assert: aDateAndTime day =   DateAndTime new day

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayMonthYearDo [
	|iterations|
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  iterations := iterations + 1])  = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear])  = 1970.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) = 1.

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth  = 1.

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek  = 5.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Thu'.
	self assert: aDateAndTime dayOfWeekName = 'Thursday'.

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear  = 1.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth  = 31.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear  = 365.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear  = 364.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testDuration [
	self assert: aDateAndTime duration  = 0 asDuration.


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testEpoch [
	self assert: aDateAndTime =  '1970-01-01T00:00:00+00:00' asDateAndTime

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFirstDayOfMonth [
	self assert: aDateAndTime firstDayOfMonth =   1

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFromSeconds [
	self assert: aDateAndTime =  (DateAndTime fromSeconds: 2177452800).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testFromString [
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00').

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHash [
	self assert: aDateAndTime hash = (DateAndTime year: 1970 month: 1 day: 1) hash
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHour [
	self assert: aDateAndTime hour =    aDateAndTime hour24.
	self assert: aDateAndTime hour =    0.
	self assert: aDateAndTime hour =    aDateAndTime hours

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testHour12 [
	self assert: aDateAndTime hour12  = DateAndTime new hour12.
	self assert: aDateAndTime hour12  = 12

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testIsLeapYear [
	self deny: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testJulianDayNumber [
	self assert: aDateAndTime =  (DateAndTime julianDayNumber: 2440588).
	self assert: aDateAndTime julianDayNumber = 2440588.
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation = 'AM'.

	
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) = 
	 (Timespan starting: '12-31-1969' asDate duration: 2 days).
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMidnight [
	self assert: aDateAndTime midnight =  aDateAndTime

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMinus [
	self assert: aDateAndTime - aDateAndTime =  '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration = aDateAndTime.
	self assert: aDateAndTime - aDuration =  (DateAndTime year: 1969 month: 12 day: 30 hour: 21 minute: 56 second: 55 nanoSecond: 999999995 offset: 0 hours ).
	" I believe this Failure is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)" 
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMinute [
	self assert: aDateAndTime minute =  0


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMinutes [
	self assert: aDateAndTime minutes = 0

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testMonth [
	self assert: aDateAndTime month  = 1.
	self assert: aDateAndTime monthAbbreviation = 'Jan'.
	self assert: aDateAndTime monthName = 'January'.
	self assert: aDateAndTime monthIndex = 1.
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond =  0


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNoon [
	self assert: aDateAndTime noon = '1970-01-01T12:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testNow [
	self deny: aDateAndTime =  (DateAndTime now).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testOffset [
	self assert: aDateAndTime offset =  '0:00:00:00' asDuration.
     self assert: (aDateAndTime offset: '0:12:00:00') equals:  '1970-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testPlus [
	self assert: aDateAndTime + '0:00:00:00' = aDateAndTime.
	self assert: aDateAndTime + 0 = aDateAndTime.
	self assert: aDateAndTime + aDuration = (DateAndTime year: 1970 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours )
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
	

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1970-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(UTZ)'
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testSecond [
	self assert: aDateAndTime second =  0


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testSeconds [
	self assert: aDateAndTime seconds =  0


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTicks [
	self assert: aDateAndTime ticks =  (DateAndTime julianDayNumber: 2440588) ticks.
	self assert: aDateAndTime ticks = #(2440588 0 0)
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTicksOffset [
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2415386 0 0) offset: DateAndTime localOffset).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTimeZone [
	self assert: aDateAndTime timeZoneName	= 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation	=  'UTC'


]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTo [
	self assert: (aDateAndTime to: aDateAndTime) = ((DateAndTime year: 1970 month: 1 day: 1) to: (DateAndTime year: 1970 month: 1 day: 1)) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToBy [
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) = 
				((DateAndTime year: 1970 month: 1 day: 1) to:
				 (DateAndTime year: 1970 month: 1 day: 1) + 10 days by: 5 days ) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToByDo [
	"self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days do: []) =  "
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testToday [
	self deny: aDateAndTime =  (DateAndTime today).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testTommorrow [
	self assert: (DateAndTime today + 24 hours) =  (DateAndTime tomorrow).
	self deny: aDateAndTime =  (DateAndTime tomorrow).
     "MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testUtcOffset [
     self assert: (aDateAndTime offset: '0:12:00:00') =  '1970-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYear [
	self assert: aDateAndTime year = 1970.

	
]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearDay [
	self assert: aDateAndTime =  (DateAndTime year: 1970 day: 1).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearDayHourMinuteSecond [
	self assert: aDateAndTime =  (DateAndTime year: 1970 day: 1 hour: 0 minute: 0 second: 0).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDay [
	self assert: aDateAndTime =  (DateAndTime year: 1970 month: 1 day: 1).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDayHourMinuteSecond [
	self assert: aDateAndTime =  (DateAndTime year: 1970 month: 1 day: 1 hour: 0 minute: 0 second: 0).

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYearMonthDayHourMinuteSecondNanosSecondOffset [
	self assert: aDateAndTime =  (DateAndTime year: 1970 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset:0 hours ).
	self assert: ((DateAndTime year: 1 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset: 0 hours ) +
				(Duration days: 1 hours: 2 minutes: 3 seconds: 4  nanoSeconds: 5) ) =  	
				(DateAndTime year: 1 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours ) 
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"   

]

{ #category : #tests }
DateAndTimeUnixEpochTest >> testYesterday [
	self deny: aDateAndTime =  (DateAndTime yesterday).

]
