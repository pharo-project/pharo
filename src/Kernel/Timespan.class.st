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
	#category : #'Kernel-Chronology'
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

	^ self species starting: (self start + operand) duration: self duration

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
Timespan >> asDuration [

	^ self duration
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

{ #category : #accessing }
Timespan >> dayOfMonth [
	"Answer the day of the month represented by the receiver."

	^ start dayOfMonth
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

{ #category : #comparing }
Timespan >> hash [

	^ start hash + duration hash 
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
Timespan >> monthIndex [

	^ self month 
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

{ #category : #accessing }
Timespan >> year [ 

	^ start year 
]
