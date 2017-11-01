"
I represent one of several Sunit test Cases intentended to provide complete coverage for the Chronology set of classes as part of the external testing. tlk.
My fixtures are:
aDateAndTime = February 29, 2004 1:33 PM with offset: 2 hours
aDuration = 15 days, 14 hours, 13 minutes, 12 seconds and 11 nano seconds.
aTimeZone =  Grenwhich Meridian (local offset = 0 hours) 
"
Class {
	#name : #DateAndTimeLeapTest,
	#superclass : #TestCase,
	#instVars : [
		'aDateAndTime',
		'aDuration',
		'aTimeZone',
		'localTimeZoneToRestore'
	],
	#category : 'Kernel-Tests-Chronology'
}

{ #category : #running }
DateAndTimeLeapTest >> setUp [
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
	self assert: aDateAndTime asLocal =  aDateAndTime.
	self assert: aDateAndTime asLocal = (aDateAndTime offset: aDateAndTime class localOffset)

	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsMonth [
	self
		assert: aDateAndTime asMonth
		equals: (Month year: 2004 month: 'February').

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsNanoSeconds [
	self assert: aDateAndTime asNanoSeconds =  aDuration asNanoSeconds.
	self assert: aDateAndTime asNanoSeconds = 48780000000000
	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsSeconds [
	self assert: aDuration asSeconds =  48780.
	self assert: aDateAndTime asSeconds = 3255507180.
	

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsTime [
	self assert: aDateAndTime asTime equals: (Time hour: 13 minute: 33 second: 0)

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsUTC [
	self assert: aDateAndTime asUTC =  aDateAndTime

          
]

{ #category : #tests }
DateAndTimeLeapTest >> testAsWeek [
	self assert: aDateAndTime asWeek equals: ((Week starting: '02-29-2004' asDate) translateTo: 2 hours).

]

{ #category : #tests }
DateAndTimeLeapTest >> testAsYear [
	"A year always starts at January 1"
	self 
		assert: aDateAndTime asYear
		equals: ((Year starting: '02-29-2004' asDate) translateTo: 2 hours ).  
	self
		assert: aDateAndTime asYear
		equals: ((Year starting: '01-01-2004' asDate) translateTo: 2 hours)  

]

{ #category : #tests }
DateAndTimeLeapTest >> testDay [
	self assert: aDateAndTime day =   60. 
	self deny: aDateAndTime day =   29 
]

{ #category : #tests }
DateAndTimeLeapTest >> testDayMonthYearDo [
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachYear])  = 2004.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachMonth]) = 2.
	self assert: (aDateAndTime dayMonthYearDo: [:eachDay :eachMonth :eachYear |  eachDay]) = 29.

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfMonth [
	self assert: aDateAndTime dayOfMonth  = 29.

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfWeek [
	self assert: aDateAndTime dayOfWeek  = 1.
	self assert: aDateAndTime dayOfWeekAbbreviation = 'Sun'.
	self assert: aDateAndTime dayOfWeekName = 'Sunday'.

]

{ #category : #tests }
DateAndTimeLeapTest >> testDayOfYear [
	self assert: aDateAndTime dayOfYear  = 60.


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysInMonth [
	self assert: aDateAndTime daysInMonth  = 29.


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysInYear [
	self assert: aDateAndTime daysInYear  = 366.


]

{ #category : #tests }
DateAndTimeLeapTest >> testDaysLeftInYear [
	self assert: aDateAndTime daysLeftInYear  = 306.


]

{ #category : #tests }
DateAndTimeLeapTest >> testFirstDayOfMonth [
	self deny: aDateAndTime firstDayOfMonth =  1.
	self assert: aDateAndTime firstDayOfMonth = 32

]

{ #category : #tests }
DateAndTimeLeapTest >> testFromString [
	self assert: aDateAndTime =  (DateAndTime fromString: ' 2004-02-29T13:33:00+02:00').


]

{ #category : #tests }
DateAndTimeLeapTest >> testHour [
	self assert: aDateAndTime hour equals: aDateAndTime hour24.
	self assert: aDateAndTime hour equals: 13.
	self assert: aDateAndTime hour equals: aDateAndTime hours

]

{ #category : #tests }
DateAndTimeLeapTest >> testHour12 [
	self assert: aDateAndTime hour12  =   1.

]

{ #category : #tests }
DateAndTimeLeapTest >> testIsLeapYear [
	self assert: aDateAndTime isLeapYear

]

{ #category : #tests }
DateAndTimeLeapTest >> testLessThan [
	self assert: aDateAndTime  < (aDateAndTime + '1:00:00:00').
	self assert: aDateAndTime + -1 < aDateAndTime.
	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMeridianAbbreviation [
	self assert: aDateAndTime meridianAbbreviation = 'PM'.

	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMiddleOf [
	self assert: (aDateAndTime middleOf: aDuration)  = 
	 (Timespan starting: (DateAndTime year: 2004 month: 2 day: 29 hour: 6 minute: 46 second: 30 offset: 2 hours)
	duration: (Duration days: 0 hours: 13 minutes: 33 seconds: 0 nanoSeconds: 0 ))
	
]

{ #category : #tests }
DateAndTimeLeapTest >> testMidnight [
	self assert: aDateAndTime midnight equals: '2004-02-29T00:00:00+02:00' asDateAndTime.

]

{ #category : #tests }
DateAndTimeLeapTest >> testMinute [
	self assert: aDateAndTime minute =  33


]

{ #category : #tests }
DateAndTimeLeapTest >> testMinutes [
	self assert: aDateAndTime minutes = 33

]

{ #category : #tests }
DateAndTimeLeapTest >> testMonth [
	self assert: aDateAndTime month  = 2.
	self assert: aDateAndTime monthAbbreviation = 'Feb'.
	self assert: aDateAndTime monthName = 'February'.
	self assert: aDateAndTime monthIndex = 2.
]

{ #category : #tests }
DateAndTimeLeapTest >> testNanoSecond [
	self assert: aDateAndTime nanoSecond =  0


]

{ #category : #tests }
DateAndTimeLeapTest >> testNoon [
	self assert: aDateAndTime noon equals:  '2004-02-29T12:00:00+02:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testOffset [

	self assert: aDateAndTime offset =  '0:02:00:00' asDuration.
	self assert: (aDateAndTime offset: '-0:12:00:00') equals: '2004-02-28T23:33:00-12:00' asDateAndTime.
	self assert: (aDateAndTime offset: '0:12:00:00') equals:  '2004-02-29T23:33:00+12:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aDateAndTime printOn: str ]) equals: '2004-02-29T13:33:00+02:00'.
	self assert: (String streamContents: [ :str | aTimeZone printOn: str ]) equals: 'a TimeZone(UTC)'
]

{ #category : #tests }
DateAndTimeLeapTest >> testSecond [
	self assert: aDateAndTime second =  0


]

{ #category : #tests }
DateAndTimeLeapTest >> testSeconds [
	self assert: aDateAndTime seconds =  0


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
	self assert: aDateAndTime =  (aDateAndTime ticks:  #(2453065 48780 0) offset: DateAndTime localOffset).


]

{ #category : #tests }
DateAndTimeLeapTest >> testTimeZone [
	aDateAndTime := '2004-02-29T13:33:00+00:00' asDateAndTime.
	self assert: aDateAndTime timeZone name equals: 'Universal Time'.
	self assert: aDateAndTime timeZone abbreviation 	equals: 'UTC'


]

{ #category : #tests }
DateAndTimeLeapTest >> testUtcOffset [
     self assert: (aDateAndTime offset: '0:02:00:00') =  '2004-02-29T13:33:00+02:00' asDateAndTime
]

{ #category : #tests }
DateAndTimeLeapTest >> testYear [
	self assert: aDateAndTime year = 2004.

	
]

{ #category : #tests }
DateAndTimeLeapTest >> testYearDayHourMinuteSecond [
	self assert: aDateAndTime equals:  (DateAndTime year: 2004 day: 60 hour: 13 minute: 33 second: 0 offset: 2 hours).

]

{ #category : #tests }
DateAndTimeLeapTest >> testYearMonthDayHourMinuteSecond [
	self assert: aDateAndTime equals:  (DateAndTime year: 2004 month: 2 day: 29 hour: 13 minute: 33 second: 0 offset: 2 hours).

]
