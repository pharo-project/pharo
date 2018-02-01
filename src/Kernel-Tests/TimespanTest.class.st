"
SUnit tests for Timespan class
"
Class {
	#name : #TimespanTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'timespan',
		'aTimespan',
		'anOverlappingTimespan',
		'anIncludedTimespan',
		'aDisjointTimespan',
		'aDay',
		'aWeek',
		'dec31',
		'jan01',
		'jan08',
		'localTimeZoneToRestore'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
TimespanTest >> classToBeTested [

	^ Timespan

]

{ #category : #running }
TimespanTest >> setUp [
	super setUp.
	localTimeZoneToRestore := DateAndTime localTimeZone.
	DateAndTime localTimeZone: TimeZone default.

	"100 hours starting noon 22 March 2003"
	timespan := Timespan starting:
					(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0)
						duration: (Duration hours: 100).

	dec31 := (DateAndTime year: 2004 month: 12 day: 31 hour: 0 minute: 0 second: 0).
	jan01 := (DateAndTime year: 2005 month: 1 day: 1 hour: 0 minute: 0 second: 0).
	jan08 := (DateAndTime year: 2005 month: 1 day: 8 hour: 0 minute: 0 second:0).
	aDay := Duration days: 1 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0.
	aWeek := Duration days: 7 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0.
	aTimespan := Timespan starting: jan01 duration: aWeek.
	anOverlappingTimespan := Timespan starting: dec31 duration: aWeek.
	anIncludedTimespan := Timespan starting: jan01 duration: aDay.
	aDisjointTimespan := Timespan starting: jan08 duration: aWeek.




]

{ #category : #running }
TimespanTest >> tearDown [
	DateAndTime localTimeZone: localTimeZoneToRestore.
	timespan := nil.
	super tearDown 

]

{ #category : #tests }
TimespanTest >> testAccessing [
	self
		assert: timespan start
			equals:
			(DateAndTime
				year: 2003
				month: 03
				day: 22
				hour: 12
				minute: 0
				second: 0);
		assert: timespan duration equals: (Duration hours: 100);
		assert: timespan month equals: 3;
		assert: timespan monthName equals: 'March';
		assert: timespan monthAbbreviation equals: 'Mar'
]

{ #category : #tests }
TimespanTest >> testArithmetic [
	| ts1 ts2 d |

	ts1 := timespan + 2 days.
	ts2 := ts1 - 2 days.
	d := ts1
		-
			(DateAndTime
				year: 2003
				month: 03
				day: 20).

	self
		assert: ts1 start
			equals:
			(DateAndTime
				year: 2003
				month: 03
				day: 24
				hour: 12
				minute: 0
				second: 0);
		assert: ts1 duration equals: timespan duration;
		assert: ts2 start equals: timespan start;
		assert: ts2 duration equals: timespan duration.

	self
		assert: d
		equals:
			(Duration
				days: 4
				hours: 12
				minutes: 0
				seconds: 0)
]

{ #category : #tests }
TimespanTest >> testAsDate [
	self assert: aTimespan asDate equals: jan01 asDate
	"MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
TimespanTest >> testAsDateAndTime [
	self assert: aTimespan asDateAndTime equals: jan01
	"MessageNotUnderstood: Date class>>starting:"
]

{ #category : #tests }
TimespanTest >> testAsDuration [
	self assert: aTimespan asDuration equals: aWeek
]

{ #category : #tests }
TimespanTest >> testAsMonth [
	self assert: aTimespan asMonth equals: jan01 asMonth
]

{ #category : #tests }
TimespanTest >> testAsTime [
	self assert: aTimespan asTime equals: jan01 asTime
	"MessageNotUnderstood: Time class>>seconds:nanoSeconds:"
]

{ #category : #tests }
TimespanTest >> testAsWeek [
	self assert: aTimespan asWeek equals: jan01 asWeek
	"DateAndTime new asWeek
	MessageNotUnderstood: Week class>>starting:"
]

{ #category : #tests }
TimespanTest >> testAsYear [
	self assert: aTimespan asYear equals: jan01 asYear
]

{ #category : #tests }
TimespanTest >> testClockPrecisionDuration [
	| ts |

	ts := Timespan starting: Date today duration: DateAndTime clockPrecision.
	self assert: ts start equals: ts end
]

{ #category : #tests }
TimespanTest >> testCurrent [
	self assert: (Timespan starting: DateAndTime current) <= Timespan current.
	self assert: Timespan current <= (Timespan starting: DateAndTime current)
]

{ #category : #tests }
TimespanTest >> testDay [
	self assert: aTimespan day equals: jan01 day
]

{ #category : #tests }
TimespanTest >> testDayOfMonth [
	self assert: aTimespan dayOfMonth equals: 1
]

{ #category : #tests }
TimespanTest >> testDayOfWeek [
	self assert: aTimespan dayOfWeek equals: 7.
	self assert: aTimespan dayOfWeekName equals: 'Saturday'
]

{ #category : #tests }
TimespanTest >> testDayOfYear [
	self assert: aTimespan dayOfYear equals: 1
	"MessageNotUnderstood: UndefinedObject>>year:, Undefined object is Year class"
]

{ #category : #tests }
TimespanTest >> testDaysInMonth [
	self assert: aTimespan daysInMonth equals: 31
	"MessageNotUnderstood: Month class>>starting:"
]

{ #category : #tests }
TimespanTest >> testDaysInYear [
	self assert: aTimespan daysInYear equals: 365
	"MessageNotUnderstood: UndefinedObject>>starting:  UndefinedObject is Year class"
]

{ #category : #tests }
TimespanTest >> testDaysLeftInYear [
	self assert: aTimespan daysLeftInYear equals: 364
	"MessageNotUnderstood: UndefinedObject>>starting:  UndefinedObject is Year class"
]

{ #category : #tests }
TimespanTest >> testDoWith [
	| count |

	count := 0.
	aTimespan do: [ :each | count := count + 1 ] with: (Timespan starting: jan01 duration: aDay).
	self assert: count equals: 7
]

{ #category : #tests }
TimespanTest >> testDoWithWhen [
	| count |

	count := 0.
	aTimespan
		do: [ :each | count := count + 1 ]
		with: (Timespan starting: jan01 duration: aDay)
		when: [ :each | count < 5 ].
	self assert: count equals: 5
]

{ #category : #tests }
TimespanTest >> testDuration [
	self assert: aTimespan duration equals: aWeek.
	aTimespan duration: aDay.
	self assert: aTimespan duration equals: aDay
]

{ #category : #tests }
TimespanTest >> testEnd [
	self assert: aTimespan end + (Duration nanoSeconds: 1) equals: aDisjointTimespan start.
	self
		assert: aTimespan end
		equals:
			(DateAndTime
				year: 2005
				month: 1
				day: 7
				hour: 23
				minute: 59
				second: 59
				nanoSecond: 999999999
				offset: 0 hours)
]

{ #category : #tests }
TimespanTest >> testEveryDo [
	| count duration |

	count := 0.
	duration := 7 days.
	aTimespan every: duration do: [ :each | count := count + 1 ].
	self assert: count equals: 1
]

{ #category : #tests }
TimespanTest >> testFirstDayOfMonth [
	self assert: aTimespan firstDayOfMonth equals: 1.
	self assert: aDisjointTimespan firstDayOfMonth equals: 1
]

{ #category : #tests }
TimespanTest >> testIncludes [
	self assert: (aTimespan includes: jan01).
	self deny: (aTimespan includes: jan08)

]

{ #category : #tests }
TimespanTest >> testIncludesAll [
	self assert: (aTimespan includesAll: (Bag with: jan01)).
	self deny: (aTimespan includesAll: (Bag with: jan01 with: jan08))

]

{ #category : #tests }
TimespanTest >> testIncludesAny [
	self deny: (aTimespan includesAnyOf: (Bag with: dec31)).
	self assert: (aTimespan includesAnyOf: (Bag with: jan01 with: jan08))
	"Error is due to bug in Timespan 
includesAnyOf: aCollection "
	"Answer whether any element of aCollection is included in the receiver"
	"aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].
Shouldn't this return false if none are included?
"

]

{ #category : #tests }
TimespanTest >> testInclusion [

	| t1 t2 t3 t4 |
	t1 := timespan start.
	t2 := timespan start + (timespan duration / 2).
	t3 := timespan end.
	t4 := timespan start + (timespan duration).

	self 
		assert: (timespan includes: t1);
		assert: (timespan includes: t2);
		assert: (timespan includes: t3)";
		deny: (timespan includes: t4).
	self
		assert: (timespan includes: (t1 to: t2));
		assert: (timespan includes: (t1 to: t4));
		deny: (timespan includes: (Timespan starting: t2 duration: (timespan duration * 2))).
	self 
		assert: (timespan includesAllOf: { t1. t2. t3 } );
		deny: (timespan includesAllOf: { t1. t2. t3. t4} ).
	self 
		assert: (timespan includesAnyOf: { t1. t2. t3 } );
		deny: (timespan includesAnyOf: { t4 } ).
"
]

{ #category : #tests }
TimespanTest >> testIntersectionWithDisjoint [
	self assert: (aTimespan intersection: aDisjointTimespan) isNil.

]

{ #category : #tests }
TimespanTest >> testIntersectionWithIncluded [
	self
		assert: (aTimespan intersection: anIncludedTimespan)
		equals:
			(Timespan
				starting: jan01
				duration:
					(Duration
						days: 0
						hours: 23
						minutes: 59
						seconds: 59
						nanoSeconds: 999999999)).
	self deny: (aTimespan intersection: anIncludedTimespan) equals: anIncludedTimespan
]

{ #category : #tests }
TimespanTest >> testIntersectionWithOverlapping [
	self
		assert: (aTimespan intersection: anOverlappingTimespan)
		equals:
			(Timespan
				starting: jan01
				duration:
					(Duration
						days: 5
						hours: 23
						minutes: 59
						seconds: 59
						nanoSeconds: 999999999))
]

{ #category : #tests }
TimespanTest >> testIntersectionWithSelf [
	self
		assert: (aTimespan intersection: aTimespan)
		equals:
			(Timespan
				starting: jan01
				duration:
					(Duration
						days: 6
						hours: 23
						minutes: 59
						seconds: 59
						nanoSeconds: 999999999)).
	self deny: (aTimespan intersection: anIncludedTimespan) equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testIntersectionWithSeparate [
	self assert: (aTimespan intersection: aDisjointTimespan) isNil.
	self deny: (aTimespan intersection: anOverlappingTimespan) isNil.
	self
		assert: (aTimespan intersection: anIncludedTimespan)
		equals:
			(Timespan
				starting: jan01
				duration:
					(Duration
						days: 0
						hours: 23
						minutes: 59
						seconds: 59
						nanoSeconds: 999999999)).
	self deny: (aTimespan intersection: anIncludedTimespan) equals: anIncludedTimespan
]

{ #category : #tests }
TimespanTest >> testIsLeapYear [
	"self assert: anOverlappingTimespan isLeapYear."
	"not sure why this fails"
	self deny: aTimespan isLeapYear

]

{ #category : #tests }
TimespanTest >> testJulianDayNumber [
	self assert: aTimespan julianDayNumber equals: jan01 julianDayNumber
]

{ #category : #tests }
TimespanTest >> testLessThan [
	self assert: aTimespan  < aDisjointTimespan.
	self deny: anIncludedTimespan < aTimespan
	
]

{ #category : #tests }
TimespanTest >> testMinusADateAndTime [
	"It appears that subtracting a date from a Timespan gives you a duration = to the difference between the start of the timespan and the date "

	self assert: aTimespan - dec31 equals: aDay.
	self assert: aDisjointTimespan - jan01 equals: aWeek
]

{ #category : #tests }
TimespanTest >> testMinusADuration [
	"It appears that subtracting a duration from a Timespan gives you a Timespan shifted by the duration"

	self assert: aTimespan - aDay equals: anOverlappingTimespan.
	self assert: aDisjointTimespan - aWeek equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testMonth [
	self assert: aTimespan month equals: 1.
	self assert: aTimespan monthName equals: 'January'.
	self assert: aTimespan monthIndex equals: 1
]

{ #category : #tests }
TimespanTest >> testNew [
	self assert: Timespan new equals: (Timespan starting: '01-01-1901' asDate)
]

{ #category : #tests }
TimespanTest >> testNext [
	self assert: aTimespan next equals: aDisjointTimespan
]

{ #category : #tests }
TimespanTest >> testPlus [
	self assert: aTimespan + aWeek equals: aDisjointTimespan.
	self assert: anOverlappingTimespan + aDay equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testPrevious [
	self assert: aTimespan equals: aDisjointTimespan previous.
	self assert: aTimespan next previous equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testPrintOn [
	self assert: (String streamContents: [ :str | aTimespan printOn: str ]) equals: 'a Timespan(2005-01-01T00:00:00+00:00D7:00:00:00)'
]

{ #category : #tests }
TimespanTest >> testStart [
	self assert: aTimespan start equals: jan01.
	aTimespan start: jan08.
	self assert: aTimespan start equals: jan08
]

{ #category : #tests }
TimespanTest >> testStartingEnding [
	self assert: aTimespan equals: (Timespan starting: jan01 ending: jan08)
]

{ #category : #tests }
TimespanTest >> testTo [
	self assert: (anIncludedTimespan to: jan08) equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testUnion [
	| union |

	union := timespan union: timespan.

	self
		assert: union start equals: timespan start;
		assert: union duration equals: timespan duration
]

{ #category : #tests }
TimespanTest >> testUnionWithDisjoint [
	self assert: (aTimespan union: aDisjointTimespan) equals: (Timespan starting: jan01 duration: 14 days)
]

{ #category : #tests }
TimespanTest >> testUnionWithIncluded [
	self assert: (aTimespan union: anIncludedTimespan) equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testUnionWithOverlapping [
	self assert: (aTimespan union: anOverlappingTimespan) equals: (Timespan starting: dec31 duration: 8 days)
]

{ #category : #tests }
TimespanTest >> testUnionWithSelf [
	self assert: (aTimespan union: aTimespan) equals: aTimespan
]

{ #category : #tests }
TimespanTest >> testUnionWithSeparate [
	self
		assert: (anOverlappingTimespan union: aDisjointTimespan)
		equals: (Timespan starting: anOverlappingTimespan start ending: aDisjointTimespan end + DateAndTime clockPrecision)
]

{ #category : #tests }
TimespanTest >> testWorkDatesDo [
	| count |

	count := 0.
	aTimespan workDatesDo: [ :each | count := count + 1 ].
	self assert: count equals: 5
]

{ #category : #tests }
TimespanTest >> testYear [
	self assert: aTimespan year equals: 2005
]

{ #category : #tests }
TimespanTest >> testZeroDuration [
	| ts |

	ts := Timespan starting: Date today duration: Duration zero.
	self assert: ts start equals: ts end
]
