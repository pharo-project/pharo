"
I represent a week.
"
Class {
	#name : #Week,
	#superclass : #Timespan,
	#classVars : [
		'StartDay'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #accessing }
Week class >> dayNames [

	^ DayNames 
]

{ #category : #accessing }
Week class >> indexOfDay: aSymbol [

	^ DayNames indexOf: aSymbol 
]

{ #category : #accessing }
Week class >> nameOfDay: anIndex [

	^ DayNames at: anIndex 
]

{ #category : #accessing }
Week class >> startDay [

	^ StartDay ifNil: [ StartDay := DayNames first ]
]

{ #category : #accessing }
Week class >> startDay: aSymbol [

	(DayNames includes: aSymbol)
		ifTrue: [ StartDay := aSymbol ]
		ifFalse: [ self error: aSymbol, ' is not a recognised day name' ]

]

{ #category : #'instance creation' }
Week class >> starting: aDateAndTime duration: aDuration [
	"Override - the duration is always one week.
	 Week will start from the Week class>>startDay"

	| midnight delta adjusted |
	midnight := aDateAndTime asDateAndTime midnight.
	delta := ((midnight dayOfWeek + 7 - (DayNames indexOf: self startDay)) rem: 7) abs.
	adjusted := midnight - (Duration days: delta seconds: 0).

	^ super
		starting: adjusted
		duration: (Duration weeks: 1).
]

{ #category : #'instance creation' }
Week class >> week: weekNumber [
	"Return a week in this year"
	^ self year: Year current week: weekNumber
]

{ #category : #'instance creation' }
Week class >> year: aYear week: weekNumber [
	"Return the ISO week for the given week number and the given year.
	Week 1 contains the year's first Thursday (weekday = 5)"
	^ self starting: aYear asYear firstThursday + (weekNumber * 7 - 5"adjust for firstThursday") days
]

{ #category : #conversion }
Week >> asWeek [

	^ self 
]

{ #category : #accessing }
Week >> index [

	^ self asMonth dayOfWeek + self dayOfMonth - 2  // 7 + 1

]

{ #category : #printing }
Week >> printOn: aStream [

	aStream nextPutAll: 'a Week starting: '.
	self start printOn: aStream. 
]
