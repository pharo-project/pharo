"
I am one of several Sunit test Cases intentended to provide complete coverage for the Chronology set of classes as part of the external testing. See DateAndTimeEpochTestCase for a complete list.  tlk.
My fixtures are:
aDate = January 8, 2003, midnight
aDuration = 91 days
aTimeSpan= 91 days, starting January 8, 2003, midnight

"
Class {
	#name : #TimespanDoTest,
	#superclass : #TestCase,
	#instVars : [
		'aTimespan',
		'aDuration',
		'aDate'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #running }
TimespanDoTest >> setUp [
	super setUp.
	aDate := DateAndTime
				year: 2003
				month: 01
				day: 07
				hour: 0
				minute: 0
				second: 0.
	aDuration := Duration
				days: 91
				hours: 0
				minutes: 0
				seconds: 0
				nanoSeconds: 0.
	aTimespan := Timespan starting: aDate duration: aDuration
]

{ #category : #tests }
TimespanDoTest >> testDatesDo [
	| dateArray |
	dateArray := OrderedCollection new.
	7
		to: 97
		do: [:each | dateArray
				addLast: (Date year: 2003 day: each)].
	dateArray := dateArray asArray.
	self assert: aTimespan dates = dateArray
]

{ #category : #tests }
TimespanDoTest >> testDoWith [
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan
				starting: aDate
				duration: 7 days).
	self assert: count = 13
]

{ #category : #tests }
TimespanDoTest >> testDoWithWhen [
	| count |
	count := 0.
	aTimespan
		do: [:each | count := count + 1]
		with: (Timespan starting: aDate duration: 7 days)
		when: [:each | count < 5].
	self assert: count = 5	

]

{ #category : #tests }
TimespanDoTest >> testEveryDo [
	|count  duration |
	count := 0.
	duration := 7 days.
	(aTimespan
			every: duration
			do: [:each | count := count + 1]).
	self assert: count = 13
			
]

{ #category : #tests }
TimespanDoTest >> testMonthsDo [
	| monthArray |
	monthArray := Array
				with: (Month
						starting: (DateAndTime year: 2003 day: 1)
						duration: 31 days)
				with: (Month
						starting: (DateAndTime year: 2003 day: 32)
						duration: 28 days)
				with: (Month
						starting: (DateAndTime year: 2003 day: 60)
						duration: 31 days)		
				with: (Month
						starting: (DateAndTime year: 2003 day: 91)
						duration: 30 days).
	self assert: aTimespan months = monthArray
]

{ #category : #tests }
TimespanDoTest >> testNext [
	self assert: aTimespan next 
			= (Timespan
					starting: (DateAndTime
							year: 2003
							month: 4
							day: 8
							hour: 0
							minute: 0
							second: 0)
					duration: aDuration)
]

{ #category : #tests }
TimespanDoTest >> testWeeksDo [
	| weekArray |
	weekArray := OrderedCollection new.
	7
		to: 98
		by: 7
		do: [:each | weekArray
				addLast: (Week
						starting: (DateAndTime year: 2003 day: each)
						duration: 7 days)].
	weekArray := weekArray asArray.
	self assert: aTimespan weeks = weekArray

]

{ #category : #tests }
TimespanDoTest >> testYearsDo [
	| yearArray |
	yearArray := Array
				with: (Year
						starting: (DateAndTime year: 2003 day: 7)
						duration: 365 days).
	self assert: aTimespan years contents = yearArray contents
]
