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
	7 to: 97 do: [ :each | dateArray addLast: (Date year: 2003 day: each) ].
	dateArray := dateArray asArray.
	self assert: aTimespan dates equals: dateArray
]

{ #category : #tests }
TimespanDoTest >> testDoWith [
	| count |
	count := 0.
	aTimespan do: [ :each | count := count + 1 ] with: (Timespan starting: aDate duration: 7 days).
	self assert: count equals: 13
]

{ #category : #tests }
TimespanDoTest >> testDoWithWhen [
	| count |
	count := 0.
	aTimespan do: [ :each | count := count + 1 ] with: (Timespan starting: aDate duration: 7 days) when: [ :each | count < 5 ].
	self assert: count equals: 5
]

{ #category : #tests }
TimespanDoTest >> testEveryDo [
	| count duration |
	count := 0.
	duration := 7 days.
	aTimespan every: duration do: [ :each | count := count + 1 ].
	self assert: count equals: 13
]

{ #category : #tests }
TimespanDoTest >> testNext [
	self
		assert: aTimespan next
		equals:
			(Timespan
				starting:
					(DateAndTime
						year: 2003
						month: 4
						day: 8
						hour: 0
						minute: 0
						second: 0)
				duration: aDuration)
]
