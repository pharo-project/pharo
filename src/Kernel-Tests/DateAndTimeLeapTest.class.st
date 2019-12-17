"
I represent one of several Sunit test Cases intentended to provide complete coverage for the Chronology set of classes. 

My fixtures are:
aDateAndTime = February 29, 2004 1:33 PM with offset: 2 hours
aDuration = 15 days, 14 hours, 13 minutes, 12 seconds and 11 nano seconds.
aTimeZone = Grenwhich Meridian (local offset = 0 hours) 
"
Class {
	#name : #DateAndTimeLeapTest,
	#superclass : #TestCase,
	#instVars : [
		'aDateAndTime',
		'aDuration',
		'aTimeZone'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #running }
DateAndTimeLeapTest >> setUp [
	super setUp.
	aDateAndTime := (DateAndTime 
		year: 2004 month: 2 day: 29 
		hour: 13 minute: 33 second: 0 
		offset: 2 hours).
	aTimeZone := TimeZone default.
	aDuration := Duration days: 0 hours: 13 minutes: 33 seconds: 0 nanoSeconds: 0

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsDate [
	self assert: aDateAndTime asDate equals:  ('February 29, 2004' asDate translateTo: 2 hours).
]

{ #category : #tests }
DateAndTimeLeapTest >> testAsDuration [
	self assert: aDateAndTime asDuration equals: aDuration
	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsLocal [
	self assert: aDateAndTime asLocal equals: aDateAndTime.
	self assert: aDateAndTime asLocal equals: (aDateAndTime offset: aDateAndTime class localOffset)

	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds equals: aDuration asNanoSeconds.
	self assert: aDateAndTime asNanoSeconds equals: 48780000000000
	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsSeconds [
	self assert: aDuration asSeconds equals: 48780.
	self assert: aDateAndTime asSeconds equals: 3255507180.
	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsTime [
	self assert: aDateAndTime asTime equals: (Time hour: 13 minute: 33 second: 0)

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsUTC [
	self assert: aDateAndTime asUTC equals: aDateAndTime

          
]

{ #category : #tests }
DateAndTimeLeapTest >> testDay [
	self assert: aDateAndTime day equals: 60. 
	self deny: aDateAndTime day equals: 29 
]

{ #category : #tests }
DateAndTimeLeapTest >> testDayMonthYearDo [
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear]) equals: 2004.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) equals: 2.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) equals: 29

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth equals: 29

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek equals: 1.
	self assert: aDateAndTime dayOfWeekAbbreviation equals: 'Sun'.
	self assert: aDateAndTime dayOfWeekName equals: 'Sunday'

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear equals: 60


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth equals: 29


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear equals: 366


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear equals: 306


]

{ #category : #tests }
DateAndTimeLeapTest >> testFirstDayOfMonth [
	self deny: aDateAndTime firstDayOfMonth equals: 1.
	self assert: aDateAndTime firstDayOfMonth equals: 32
]

{ #category : #tests }
DateAndTimeLeapTest >> testFromString [
	self assert: aDateAndTime equals: (DateAndTime fromString: ' 2004-02-29T13:33:00+02:00')


]

{ #category : #tests }
DateAndTimeLeapTest >> testHour [
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 13.
	self assert: aDateAndTime hour equals: aDateAndTime hours

]

{ #category : #tests }
DateAndTimeLeapTest >> testHour12 [
	self assert: aDateAndTime hour12 equals: 1

]

{ #category : #tests }
DateAndTimeLeapTest >> testIsLeapYear [
	self assert: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeLeapTest >> testLessThan [
	self assert: aDateAndTime < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime
	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation equals: 'PM'

	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: aDuration) 
		  equals: 
	 (Timespan starting: (DateAndTime year: 2004 month: 2 day: 29 hour: 6 minute: 46 second: 30 offset: 2 hours)
				  duration: (Duration days: 0 hours: 13 minutes: 33 seconds: 0 nanoSeconds: 0 ))
	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMidnight [
	self assert: aDateAndTime midnight equals: '2004-02-29T00:00:00+02:00' asDateAndTime

]

{ #category : #tests }
DateAndTimeLeapTest >> testMinute [
	self assert: aDateAndTime minute equals: 33


]

{ #category : #tests }
DateAndTimeLeapTest >> testMinutes [
	self assert: aDateAndTime minutes equals: 33

]

{ #category : #tests }
DateAndTimeLeapTest >> testMonth [
	self assert: aDateAndTime month equals: 2.
	self assert: aDateAndTime monthAbbreviation equals: 'Feb'.
	self assert: aDateAndTime monthName equals: 'February'.
	self assert: aDateAndTime monthIndex equals: 2
]

{ #category : #tests }
DateAndTimeLeapTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond equals: 0


]

{ #category : #tests }
DateAndTimeLeapTest >> testNoon [
	self assert: aDateAndTime noon equals: '2004-02-29T12:00:00+02:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testOffset [

	self assert: aDateAndTime offset equals: '0:02:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals: '2004-02-28T23:33:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals:  '2004-02-29T23:33:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testPrintOn [
	self assert: (String streamContents: [:str | aDateAndTime printOn: str ]) equals: '2004-02-29T13:33:00+02:00'.
	self assert: (String streamContents: [:str | aTimeZone printOn: str ]) equals: 'a TimeZone(UTC)'
]

{ #category : #tests }
DateAndTimeLeapTest >> testSecond [
	self assert: aDateAndTime second equals: 0


]

{ #category : #tests }
DateAndTimeLeapTest >> testSeconds [
	self assert: aDateAndTime seconds equals: 0


]

{ #category : #tests }
DateAndTimeLeapTest >> testTicks [
	self 
		assert: aDateAndTime ticks  
		equals: ((DateAndTime julianDayNumber: 2453065) + 41580 seconds) ticks.
	self 
		assert: aDateAndTime ticks 
		equals:  #(2453065 41580 0)
]

{ #category : #tests }
DateAndTimeLeapTest >> testTicksOffset [
	self assert: aDateAndTime equals: (aDateAndTime ticks:  #(2453065 48780 0) offset: DateAndTime localOffset)


]

{ #category : #tests }
DateAndTimeLeapTest >> testTimeZone [
	aDateAndTime := '2004-02-29T13:33:00+00:00' asDateAndTime.
	self assert: aDateAndTime timeZone name equals: 'Universal Time'.
	self assert: aDateAndTime timeZone abbreviation 	equals: 'UTC'


]

{ #category : #tests }
DateAndTimeLeapTest >> testUtcOffset [
     self assert: (aDateAndTime offset: '0:02:00:00') equals: '2004-02-29T13:33:00+02:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testYear [
	self assert: aDateAndTime year equals: 2004

	
]

{ #category : #tests }
DateAndTimeLeapTest >> testYearDayHourMinuteSecond [
	self assert: aDateAndTime equals: (DateAndTime year: 2004 day: 60 hour: 13 minute: 33 second: 0 offset: 2 hours).

]

{ #category : #tests }
DateAndTimeLeapTest >> testYearMonthDayHourMinuteSecond [
	self assert: aDateAndTime equals: (DateAndTime year: 2004 month: 2 day: 29 hour: 13 minute: 33 second: 0 offset: 2 hours)

]
