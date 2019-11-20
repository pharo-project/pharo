"
I am one of several Sunit test Cases intentended to provide complete coverage for the Chronology set of classes as part of the external testing. See DateAndTimeEpochTestCase for a complete list. tlk.
My fixtures include a Timespan that crosses over a year boundary:
aDate = December 25, 2004, midnight
aDuration = 91 days
aTimeSpan= 91 days, starting December 25, 2004, midnight
"
Class {
	#name : #TimespanDoSpanAYearTest,
	#superclass : #TestCase,
	#instVars : [
		'aTimespan',
		'aDuration',
		'aDate'
	],
	#category : #'Kernel-Tests-Extended-Chronology'
}

{ #category : #running }
TimespanDoSpanAYearTest >> setUp [
	super setUp.
	aDate := DateAndTime year: 2004 month: 12 day: 25 hour: 0 minute: 0 second: 0.
	aDuration := Duration days: 91 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0.

	aTimespan := Timespan starting: aDate duration: aDuration
]

{ #category : #tests }
TimespanDoSpanAYearTest >> testMonthsDo [
	| monthArray |
	monthArray := Array
		with: (Month starting: (DateAndTime year: 2004 day: 355) duration: 31 days)
		with: (Month starting: (DateAndTime year: 2005 day: 1) duration: 31 days)
		with: (Month starting: (DateAndTime year: 2005 day: 32) duration: 29 days)
		with: (Month starting: (DateAndTime year: 2005 day: 61) duration: 31 days).

	self assert: aTimespan months equals: monthArray
]

{ #category : #tests }
TimespanDoSpanAYearTest >> testNext [
	self
		assert: aTimespan next
		equals:
			(Timespan
				starting:
					(DateAndTime
						year: 2005
						month: 3
						day: 26
						hour: 0
						minute: 0
						second: 0)
				duration: aDuration)
]

{ #category : #tests }
TimespanDoSpanAYearTest >> testWeeksDo [
	| weeks weekArray |
	weeks := aTimespan weeks.
	self assert: weeks size equals: (aDuration days / 7.0) ceiling + 1.

	weekArray := OrderedCollection new.
	weekArray
		addLast: (Week starting: (DateAndTime year: 2004 month: 12 day: 19) duration: 7 days);
		addLast: (Week starting: (DateAndTime year: 2004 month: 12 day: 26) duration: 7 days).

	2 to: 79 by: 7 do: [ :i | weekArray addLast: (Week starting: (DateAndTime year: 2005 day: i) duration: 7 days) ].

	weekArray := weekArray asArray.
	self assert: aTimespan weeks equals: weekArray
]

{ #category : #tests }
TimespanDoSpanAYearTest >> testYearsDo [
	| yearArray |
	yearArray := Array
				with: (Year year: 2004)
				with: (Year year: 2005).
	self assert: aTimespan years equals: yearArray

]
