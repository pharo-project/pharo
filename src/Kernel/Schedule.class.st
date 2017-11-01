"
I represent a powerful class for implementing recurring schedules.
"
Class {
	#name : #Schedule,
	#superclass : #Timespan,
	#instVars : [
		'schedule'
	],
	#category : 'Kernel-Chronology'
}

{ #category : #enumerating }
Schedule >> between: aStart and: anEnd do: aBlock [
	"from Cuis 99"
	
	| element end i startDate |
	end := self end min: anEnd.

	element := self start.	
	"Performance optimization. Avoid going through unnecesary days if easy."
	startDate := aStart asDate.
	(startDate > element asDate and: [ self everyDayAtSameTimes ]) ifTrue: [
		element := DateAndTime date: startDate time: element asTime ].

	i := 1.
	[ element < aStart ] whileTrue: [
		element := element + (schedule at: i).
		i := i + 1.
		i > schedule size ifTrue: [i := 1]].

	[ element <= end ] whileTrue: [
		aBlock value: element.
		element := element + (schedule at: i).
		i := i + 1.
		i > schedule size ifTrue: [i := 1]]
.

]

{ #category : #enumerating }
Schedule >> dateAndTimes [

	| dateAndTimes |
	dateAndTimes := OrderedCollection new.
	self scheduleDo: [ :e | dateAndTimes add: e ].
	^ dateAndTimes asArray.
]

{ #category : #private }
Schedule >> everyDayAtSameTimes [
	"Answer false if unknown"
	
	| count |
	count := (Duration days: 1) / self scheduleDuration.
	^ count >= 1 and: [ count isInteger ]
]

{ #category : #testing }
Schedule >> includes: aDateAndTime [

	| dt |
	dt := aDateAndTime asDateAndTime.
	self scheduleDo: [ :e | e = dt ifTrue: [^true] ].
	^ false.

]

{ #category : #enumerating }
Schedule >> schedule [
	^ schedule 
]

{ #category : #enumerating }
Schedule >> schedule: anArrayOfDurations [

	schedule := anArrayOfDurations

]

{ #category : #enumerating }
Schedule >> scheduleDo: aBlock [

	self between: (self start) and: (self end) do: aBlock. 
]

{ #category : #private }
Schedule >> scheduleDuration [
	^ schedule sum
]
