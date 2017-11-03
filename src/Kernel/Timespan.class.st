"
I represent a duration starting on a specific DateAndTime.

"
Class {
	#name : #Timespan,
	#superclass : #Magnitude,
	#instVars : [
		'start',
		'duration'
	],
	#category : #Kernel-Chronology
}

{ #category : #'instance creation' }
Timespan class >> current [ 

	^ self starting: DateAndTime now 
]

{ #category : #'instance creation' }
Timespan class >> new [
	"Answer a Timespan starting on the Squeak epoch: 1 January 1901"

	^ self starting: DateAndTime new

]

{ #category : #'instance creation' }
Timespan class >> starting: aDateAndTime [ 

	^ self starting: aDateAndTime duration: Duration zero 
]

{ #category : #'instance creation' }
Timespan class >> starting: aDateAndTime duration: aDuration [

	^ self basicNew
  		start: aDateAndTime asDateAndTime;
 		duration: aDuration;
		yourself.
]

{ #category : #'instance creation' }
Timespan class >> starting: startDateAndTime ending: endDateAndTime [

	^ self 
		starting: startDateAndTime 
		duration: (endDateAndTime asDateAndTime - startDateAndTime). 
]

{ #category : #arithmetic }
Timespan >> + operand [
	"operand conforms to protocol Duration" 	

	^ self class starting: (self start + operand) duration: self duration

]

{ #category : #arithmetic }
Timespan >> - operand [
	"operand conforms to protocol DateAndTime or protocol Duration"

	^ (operand respondsTo: #asDateAndTime) 
	 	ifTrue: [ self start - operand ]
	 	ifFalse: [ self + (operand negated) ]. 
]

{ #category : #arithmetic }
Timespan >> < comparand [

	^ self start < comparand	 
]

{ #category : #comparing }
Timespan >> = comparand [
	^ self species = comparand species 
		and: [ self start = comparand start 
				and: [ self duration = comparand duration ]]

]

{ #category : #accessing }
Timespan >> asDate [ 

	^ start asDate 
]

{ #category : #accessing }
Timespan >> asDateAndTime [

	^ start 
]

{ #category : #conversion }
Timespan >> asDosTimestamp [ 

	^ start asDosTimestamp 
]

{ #category : #conversion }
Timespan >> asDuration [

	^ self duration
]

{ #category : #conversion }
Timespan >> asMonth [ 

	^ start asMonth 
]

{ #category : #conversion }
Timespan >> asSeconds [
 	"Answer the seconds since the Squeak epoch: 1 January 1901"
 
 	^ start asSeconds
]

{ #category : #conversion }
Timespan >> asTime [

	^ start asTime
]

{ #category : #conversion }
Timespan >> asWeek [

	^ start asWeek 
]

{ #category : #conversion }
Timespan >> asYear [ 

	^ start asYear
]

{ #category : #enumerating }
Timespan >> dates [


	| dates |

	dates := OrderedCollection new.
	self datesDo: [ :m | dates add: m ].
	^ dates asArray.
]

{ #category : #enumerating }
Timespan >> datesDo: aBlock [ 

	self do: aBlock with: start asDate. 
]

{ #category : #accessing }
Timespan >> day [
	"Answer the day of the year represented by the receiver."
	^ self dayOfYear
]

{ #category : #accessing }
Timespan >> dayOfMonth [
	"Answer the day of the month represented by the receiver."

	^ start dayOfMonth
]

{ #category : #accessing }
Timespan >> dayOfWeek [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeek
]

{ #category : #accessing }
Timespan >> dayOfWeekName [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeekName
]

{ #category : #accessing }
Timespan >> dayOfYear [
	"Answer the day of the year represented by the receiver."

	^ start dayOfYear
]

{ #category : #accessing }
Timespan >> daysInMonth [ 

	^ start daysInMonth 
]

{ #category : #accessing }
Timespan >> daysInYear [
 	"Answer the number of days in the month represented by the receiver."

	^ start daysInYear 
]

{ #category : #accessing }
Timespan >> daysLeftInYear [
	^ start daysLeftInYear
]

{ #category : #private }
Timespan >> do: aBlock with: aFirstElement [

 	self do: aBlock with: aFirstElement when: [ :t | true ]. 
]

{ #category : #private }
Timespan >> do: aBlock with: aFirstElement when: aConditionBlock [

	| element end |
	element := aFirstElement.
	end := self end.
	[ element start <= end ] whileTrue:
	
	[(aConditionBlock value: element)
			ifTrue: [ aBlock value: element ].
		element := element next. ]
]

{ #category : #accessing }
Timespan >> duration [
 	"Answer the Duration of this timespan"

	^ duration 
]

{ #category : #private }
Timespan >> duration: aDuration [
	"Set the Duration of this timespan"

	duration := aDuration

]

{ #category : #accessing }
Timespan >> end [ 

	^ self duration asNanoSeconds = 0
		ifTrue: [ self start ]
		ifFalse: [ self next start - DateAndTime clockPrecision ]
]

{ #category : #enumerating }
Timespan >> every: aDuration do: aBlock [

	| element end |
	element := self start.
	end := self end.
	[ element <= end ] whileTrue:
	
	[ aBlock value: element.
		element := element + aDuration. ]

]

{ #category : #accessing }
Timespan >> firstDayOfMonth [

	^ start firstDayOfMonth
]

{ #category : #comparing }
Timespan >> hash [

	^ start hash + duration hash 
]

{ #category : #testing }
Timespan >> includes: aDateAndTime [ 

	^ (aDateAndTime isKindOf: Timespan)
			ifTrue: [ 
				(self includes: aDateAndTime start)
					and: [ self includes: aDateAndTime end ] ]
			ifFalse: [ aDateAndTime asDateAndTime between: start and: self end ]

]

{ #category : #testing }
Timespan >> includesAll: aCollection [
	"Answer whether all the elements of aCollection are in the receiver."

	^ aCollection allSatisfy: [ :elem | self includes: elem ]
]

{ #category : #testing }
Timespan >> includesAllOf: aCollection [ 

	self
		deprecated: 'Please use #includesAll: instead'
		transformWith: '`@receiver includesAllOf: `@statements' -> '`@receiver includesAll: `@statements'.
	
	^ self includesAll: aCollection

]

{ #category : #testing }
Timespan >> includesAny: aCollection [
	"Answer whether any element of aCollection is included in the receiver"

	^ aCollection anySatisfy: [ :elem | self includes: elem ]
]

{ #category : #testing }
Timespan >> includesAnyOf: aCollection [ 
	
	self flag: 'use includesAny: instead'.
	
	^ self includesAny: aCollection
]

{ #category : #enumerating }
Timespan >> intersection: aTimespan [

	 "Return the Timespan both have in common, or nil"

	 | aBegin anEnd |
	 aBegin := self start max: aTimespan start.
	 anEnd := self end min: aTimespan end.
	 anEnd < aBegin ifTrue: [^nil].

	 ^ self class starting: aBegin ending: anEnd.

]

{ #category : #accessing }
Timespan >> isLeapYear [

	^ start isLeapYear 
]

{ #category : #accessing }
Timespan >> julianDayNumber [ 

	^ start julianDayNumber 
]

{ #category : #accessing }
Timespan >> julianDayNumberUTC [
	^ start julianDayNumberUTC
]

{ #category : #accessing }
Timespan >> month [

	^ start month 
]

{ #category : #accessing }
Timespan >> monthAbbreviation [ 

	^ start monthAbbreviation 
]

{ #category : #accessing }
Timespan >> monthIndex [

	^ self month 
]

{ #category : #accessing }
Timespan >> monthName [ 

	^ start monthName 
]

{ #category : #enumerating }
Timespan >> months [

	| months |
	months := OrderedCollection new: 12.
	self monthsDo: [ :m | months add: m ].
	^ months asArray.
]

{ #category : #enumerating }
Timespan >> monthsDo: aBlock [

 	self do: aBlock with: start asMonth.
]

{ #category : #accessing }
Timespan >> next [

	^ self class starting: (start + duration) duration: duration 
]

{ #category : #accessing }
Timespan >> offset [
	^ start offset
]

{ #category : #accessing }
Timespan >> offset: anOffset [
	"Answer a <Timespan> equivalent to the receiver but with its local time 
	being offset from UTC by offset.
	Unlike #translateTo: this will NOT change the absolute in UTC "

	^ self class 
		starting: (self start offset: anOffset) 
		duration: self duration
]

{ #category : #accessing }
Timespan >> previous [ 

	^ self class starting: (start - duration) duration: duration 
]

{ #category : #printing }
Timespan >> printOn: aStream [ 

	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: start;
		nextPut: $D;
		print: duration;
		nextPut: $).

]

{ #category : #accessing }
Timespan >> start [
 	"Answer the start DateAndTime of this timespan"

	^ start 
]

{ #category : #accessing }
Timespan >> start: aDateAndTime [
	"Store the start DateAndTime of this timespan"

	start := aDateAndTime asDateAndTime

]

{ #category : #conversion }
Timespan >> to: anEnd [
	"Answer an Timespan. anEnd must be aDateAndTime or a Timespan" 
	| end |
	end := (anEnd isKindOf: Timespan)
		ifTrue: [ anEnd end ]
		ifFalse: [ anEnd asDateAndTime ].
	^ Timespan starting: (self start) ending: end. 
]

{ #category : #conversion }
Timespan >> translateTo: aTimeZone [
	"Keep myself's representation and move it to another timezone offset.
	Note that unlike #offset: this WILL change the absolute time in UTC"
	
	^ self class
		starting: (start translateTo: aTimeZone)
]

{ #category : #conversion }
Timespan >> translateToUTC [ 
	" Move this represenation to UTC"
	^ self translateTo: 0
]

{ #category : #enumerating }
Timespan >> union: aTimespan [
	 "Return the Timespan spanned by both"

	| aBegin anEnd |

	aBegin := self start min: aTimespan start.
	anEnd := self end max: aTimespan end.
	^ Timespan starting: aBegin ending: (anEnd + DateAndTime clockPrecision).

]

{ #category : #enumerating }
Timespan >> weeks [


	| weeks |
	weeks := OrderedCollection new.
	self weeksDo: [ :m | weeks add: m ].
	^ weeks asArray.
]

{ #category : #enumerating }
Timespan >> weeksDo: aBlock [

	self do: aBlock with: self asWeek.
]

{ #category : #enumerating }
Timespan >> workDatesDo: aBlock [
 	"Exclude Saturday and Sunday"

	self do: aBlock with: start asDate when: [ :d | d dayOfWeek < 6 ]. 
]

{ #category : #accessing }
Timespan >> year [ 

	^ start year 
]

{ #category : #enumerating }
Timespan >> years [


	| years |
	years := OrderedCollection new.
	self yearsDo: [ :m | years add: m ].
	^ years asArray.
]

{ #category : #enumerating }
Timespan >> yearsDo: aBlock [

	self do: aBlock with: start asYear.
]
