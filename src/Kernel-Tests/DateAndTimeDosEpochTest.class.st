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
	self assert: aDateAndTime asDate =   'January 1, 1980' asDate.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsDateAndTime [
	self assert: aDateAndTime asDateAndTime =  aDateAndTime
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsDuration [
	self assert: aDateAndTime asDuration =  0 asDuration
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsLocal [
	self assert: aDateAndTime asLocal =  aDateAndTime.
	self assert: aDateAndTime asLocal = (aDateAndTime offset: aDateAndTime class localOffset)
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth 
		equals: (Month year: 1980 month: 'January'). 

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds =  0 asDuration asNanoSeconds
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsSeconds [
	self assert: aDateAndTime asSeconds = 2492985600
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsTime [
	self assert: aDateAndTime asTime =  Time midnight.

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsUTC [
	self assert: aDateAndTime asUTC =  aDateAndTime
          
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsWeek [
	self assert: aDateAndTime asWeek = (Week starting: '12-31-1979' asDate). 


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testAsYear [
	self assert: aDateAndTime asYear =   (Year starting: '01-01-1980' asDate). 

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testCurrent [
	self deny: aDateAndTime =  (DateAndTime current).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDateTime [
	self assert: aDateAndTime =  (DateAndTime date: '01-01-1980' asDate time: '00:00:00' asTime)

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDay [
	self assert: aDateAndTime day =   DateAndTime new day

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayMonthYearDo [
	|iterations|
	iterations := 0.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  iterations := iterations + 1])  = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear])  = 1980.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) = 1.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) = 1.

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth  = 1.

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek  = 3.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Tue'.
	self assert: aDateAndTime dayOfWeekName = 'Tuesday'.

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear  = 1.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth  = 31.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear  = 366.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear  = 365.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testDuration [
	self assert: aDateAndTime duration  = 0 asDuration.


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testEpoch [
	self assert: aDateAndTime =  '1980-01-01T00:00:00+00:00' asDateAndTime

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFirstDayOfMonth [
	self assert: aDateAndTime firstDayOfMonth =   1

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFromSeconds [
	self assert: aDateAndTime =  (DateAndTime fromSeconds: 2492985600).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testFromString [
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1980-01-01T00:00:00+00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1980-01-01T00:00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1980-01-01T00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1980-01-01T00:00:00+00:00').

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHash [
	self assert: aDateAndTime hash = (DateAndTime year: 1980 month: 1 day: 1) hash
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHour [
	self assert: aDateAndTime hour =    aDateAndTime hour24.
	self assert: aDateAndTime hour =    0.
	self assert: aDateAndTime hour =    aDateAndTime hours

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testHour12 [
	self assert: aDateAndTime hour12  = DateAndTime new hour12.
	self assert: aDateAndTime hour12  = 12

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testIsLeapYear [
	self assert: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testJulianDayNumber [
	self assert: aDateAndTime =  (DateAndTime julianDayNumber: 2444240).
	self assert: aDateAndTime julianDayNumber = 2444240.
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation = 'AM'.

	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: '2:00:00:00' asDuration) = 
	 (Timespan starting: '12-31-1979' asDate duration: 2 days).
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMidnight [
	self assert: aDateAndTime midnight =  aDateAndTime

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinus [
	self assert: aDateAndTime - aDateAndTime =  '0:00:00:00' asDuration.
	self assert: aDateAndTime - '0:00:00:00' asDuration = aDateAndTime.
	self assert: aDateAndTime - aDuration =  (DateAndTime year: 1979 month: 12 day: 30 hour: 21 minute: 56 second: 55 nanoSecond: 999999995 offset: 0 hours ).
	" I believe this Failure is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)" 
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinute [
	self assert: aDateAndTime minute =  0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMinutes [
	self assert: aDateAndTime minutes = 0

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testMonth [
	self assert: aDateAndTime month  = 1.
	self assert: aDateAndTime monthAbbreviation = 'Jan'.
	self assert: aDateAndTime monthName = 'January'.
	self assert: aDateAndTime monthIndex = 1.
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond =  0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNoon [
	self assert: aDateAndTime noon = '1980-01-01T12:00:00+00:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testNow [
	self deny: aDateAndTime =  (DateAndTime now).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testOffset [

	self assert: aDateAndTime offset =  '0:00:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals:  '1979-12-31T12:00:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals:  '1980-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testPlus [
	self assert: aDateAndTime + '0:00:00:00' = aDateAndTime.
	self assert: aDateAndTime + 0 = aDateAndTime.
	self assert: aDateAndTime + aDuration = (DateAndTime year: 1980 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours )
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"
	

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '1980-01-01T00:00:00+00:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(DTZ)'
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testSecond [
	self assert: aDateAndTime second =  0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testSeconds [
	self assert: aDateAndTime seconds =  0


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTicks [
	self assert: aDateAndTime ticks =  (DateAndTime julianDayNumber: 2444240) ticks.
	self assert: aDateAndTime ticks = #(2444240 0 0)
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTicksOffset [
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2444240 0 0) offset: DateAndTime localOffset).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTimeZone [
	self assert: aDateAndTime timeZoneName	= 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation	=  'UTC'


]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTo [
	self assert: (aDateAndTime to: aDateAndTime) = ((DateAndTime year: 1980 month: 1 day: 1) to: (DateAndTime year: 1980 month: 1 day: 1)) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToBy [
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) = 
				((DateAndTime year: 1980 month: 1 day: 1) to:
				 (DateAndTime year: 1980 month: 1 day: 1) + 10 days by: 5 days ) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToByDo [
	"self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days do: []) =  "
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testToday [
	self deny: aDateAndTime =  (DateAndTime today).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testTommorrow [
	self assert: (DateAndTime today + 24 hours) =  (DateAndTime tomorrow).
	self deny: aDateAndTime =  (DateAndTime tomorrow).
     "MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testUtcOffset [
     self assert: (aDateAndTime offset: '0:12:00:00')  equals: '1980-01-01T12:00:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYear [
	self assert: aDateAndTime year = 1980.

	
]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearDay [
	self assert: aDateAndTime =  (DateAndTime year: 1980 day: 1).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearDayHourMinuteSecond [
	self assert: aDateAndTime =  (DateAndTime year: 1980 day: 1 hour: 0 minute: 0 second: 0).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDay [
	self assert: aDateAndTime =  (DateAndTime year: 1980 month: 1 day: 1).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDayHourMinuteSecond [
	self assert: aDateAndTime =  (DateAndTime year: 1980 month: 1 day: 1 hour: 0 minute: 0 second: 0).

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYearMonthDayHourMinuteSecondNanosSecondOffset [
	self assert: aDateAndTime =  (DateAndTime year: 1980 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset:0 hours ).
	self assert: ((DateAndTime year: 1 month: 1 day: 1 hour: 0 minute: 0 second: 0 nanoSecond: 0 offset: 0 hours ) +
				(Duration days: 1 hours: 2 minutes: 3 seconds: 4  nanoSeconds: 5) ) =  	
				(DateAndTime year: 1 month: 1 day: 2 hour: 2 minute: 3 second: 4 nanoSecond: 5 offset: 0 hours ) 
	" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"" I believe this is a bug in the nanosecond part of (DateAndTime >> year:month:day:hour:minute:second:nanoSecond:offset:)"   

]

{ #category : #tests }
DateAndTimeDosEpochTest >> testYesterday [
	self deny: aDateAndTime =  (DateAndTime yesterday).

]
