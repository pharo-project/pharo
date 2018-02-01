"
SUnit tests for weeks
"
Class {
	#name : #WeekTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'week',
		'restoredStartDay'
	],
	#category : #'Kernel-Tests-Chronology'
}

{ #category : #coverage }
WeekTest >> classToBeTested [

	^ Week
]

{ #category : #coverage }
WeekTest >> selectorsToBeIgnored [

	| deprecated private special |

	deprecated := #().
	private := #( #printOn: ).
	special := #( #next #do: ).

	^ super selectorsToBeIgnored, deprecated, private, special.
]

{ #category : #running }
WeekTest >> setUp [
	"June 1998, 5th week"

	super setUp.
	restoredStartDay := Week startDay.
	Week startDay: #Sunday.
	week := Week starting: '4 July 1998' asDate
]

{ #category : #running }
WeekTest >> tearDown [

	Week startDay: restoredStartDay.
	week := nil.
	super tearDown 

]

{ #category : #tests }
WeekTest >> testByWeekNumber [
	"Check some week dates, assuming that the week starts on Sunday"
	week := Week year: 2013 week: 1.
	self assert: week start equals: (DateAndTime year: 2012 month: 12 day: 30).
	
	week := Week year: 2013 week: 32.
	self assert: week start equals: (DateAndTime year: 2013 month: 8 day: 4).
	
	week := Week year: 2013 week: 52.
	self assert: week start equals: (DateAndTime year: 2013 month: 12 day: 22).
	
	week := Week year: 2014 week: 1.
	self assert: week start equals: (DateAndTime year: 2013 month: 12 day: 29).
]

{ #category : #tests }
WeekTest >> testByWeekNumberInCurrentYear [
	week := Week week: 2.
	self assert: week year asYear equals: Year current.
]

{ #category : #tests }
WeekTest >> testDayNames [
	self assert: (Week dayNames) = #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
]

{ #category : #tests }
WeekTest >> testEnumerating [

	| days |
	days := OrderedCollection new.
	0 to: 6 do: [ :i | days add: ('28 June 1998' asDate addDays: i) ].

	week datesDo: [ :d | days remove: d ].
	
	self assert: days isEmpty.

]

{ #category : #tests }
WeekTest >> testIndexOfDay [
	| days |
	days := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	
	days withIndexDo: [:item :index | self assert: (Week indexOfDay: item) = index].
	
	"This should probably raise an error rather than returning 0."
	self assert: (Week indexOfDay: 0) = 0.
	self assert: (Week indexOfDay: 1) = 0.
	self assert: (Week indexOfDay: 7) = 0.
	self assert: (Week indexOfDay: 8) = 0.
	self assert: (Week indexOfDay: #Sunnyday) = 0.
]

{ #category : #tests }
WeekTest >> testInquiries [

	self
		assert: week start asDate = '28 June 1998' asDate;
		assert: week end asDate = '4 July 1998' asDate;
		assert: week index = 5;
		assert: week duration = (7 days).

]

{ #category : #tests }
WeekTest >> testNameOfDay [
	| days |
	days := #(#Sunday #Monday #Tuesday #Wednesday #Thursday #Friday #Saturday).
	
	days withIndexDo: [:item :index | self assert: (Week nameOfDay: index) = item].
	
	self should: [Week nameOfDay: 0] raise: self defaultTestError.
	self should: [Week nameOfDay: 8] raise: self defaultTestError.
	self should: [Week nameOfDay: #Sunday] raise: self defaultTestError.
]

{ #category : #tests }
WeekTest >> testOffset [
	"Check that the offset is maintained when creating a new instance of Month from a DateAndTime"

	| dt newWeek |

	dt := DateAndTime fromString: '2018/01/01T00:00:00+10'.
	newWeek := Week starting: dt duration: 0. "duration is ignored"
	self assert: newWeek asDateAndTime offset equals: dt offset
]

{ #category : #tests }
WeekTest >> testPreviousNext [
	self
		assert: week next = (Week starting: '6 July 1998' asDate);
		assert: week previous = (Week starting:  '22 June 1998' asDate)
]
