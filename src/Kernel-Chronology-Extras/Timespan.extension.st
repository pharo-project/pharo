Extension { #name : #Timespan }

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> asDosTimestamp [ 

	^ start asDosTimestamp 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> asMonth [ 

	^ start asMonth 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> asWeek [

	^ start asWeek 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> asYear [ 

	^ start asYear
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> dates [


	| dates |

	dates := OrderedCollection new.
	self datesDo: [ :m | dates add: m ].
	^ dates asArray.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> datesDo: aBlock [ 

	self do: aBlock with: start asDate. 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> day [
	"Answer the day of the year represented by the receiver."
	^ self dayOfYear
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> dayOfWeek [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeek
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> dayOfWeekName [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeekName
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> dayOfYear [
	"Answer the day of the year represented by the receiver."

	^ start dayOfYear
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> daysInMonth [ 

	^ start daysInMonth 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> daysInYear [
 	"Answer the number of days in the month represented by the receiver."

	^ start daysInYear 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> daysLeftInYear [
	^ start daysLeftInYear
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> do: aBlock with: aFirstElement [

 	self do: aBlock with: aFirstElement when: [ :t | true ]. 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> do: aBlock with: aFirstElement when: aConditionBlock [

	| element end |
	element := aFirstElement.
	end := self end.
	[ element start <= end ] whileTrue:
	
	[(aConditionBlock value: element)
			ifTrue: [ aBlock value: element ].
		element := element next. ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> end [ 

	^ self duration asNanoSeconds = 0
		ifTrue: [ self start ]
		ifFalse: [ self next start - DateAndTime clockPrecision ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> every: aDuration do: aBlock [

	| element end |
	element := self start.
	end := self end.
	[ element <= end ] whileTrue:
	
	[ aBlock value: element.
		element := element + aDuration. ]

]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> firstDayOfMonth [

	^ start firstDayOfMonth
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> includes: aDateAndTime [ 

	^ (aDateAndTime isKindOf: Timespan)
			ifTrue: [ 
				(self includes: aDateAndTime start)
					and: [ self includes: aDateAndTime end ] ]
			ifFalse: [ aDateAndTime asDateAndTime between: start and: self end ]

]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> includesAll: aCollection [
	"Answer whether all the elements of aCollection are in the receiver."

	^ aCollection allSatisfy: [ :elem | self includes: elem ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> includesAllOf: aCollection [ 

	self
		deprecated: 'Please use #includesAll: instead'
		transformWith: '`@receiver includesAllOf: `@statements' -> '`@receiver includesAll: `@statements'.
	
	^ self includesAll: aCollection

]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> includesAny: aCollection [
	"Answer whether any element of aCollection is included in the receiver"

	^ aCollection anySatisfy: [ :elem | self includes: elem ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> includesAnyOf: aCollection [ 
	^ self includesAny: aCollection
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> intersection: aTimespan [

	 "Return the Timespan both have in common, or nil"

	 | aBegin anEnd |
	 aBegin := self start max: aTimespan start.
	"Use start + duration rather than end, because the latter subtracts clockPrecision."
	 anEnd := (self start + self duration) min: (aTimespan start + aTimespan duration).
	 anEnd <= aBegin ifTrue: [^nil].

	 ^ self species starting: aBegin ending: anEnd.

]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> isLeapYear [

	^ start isLeapYear 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> monthAbbreviation [ 

	^ start monthAbbreviation 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> monthName [ 

	^ start monthName 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> months [

	| months |
	months := OrderedCollection new: 12.
	self monthsDo: [ :m | months add: m ].
	^ months asArray.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> monthsDo: aBlock [

 	self do: aBlock with: start asMonth.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> to: anEnd [
	"Answer an Timespan. anEnd must be aDateAndTime or a Timespan" 
	| end |
	end := (anEnd isKindOf: Timespan)
		ifTrue: [ anEnd end ]
		ifFalse: [ anEnd asDateAndTime ].
	^ Timespan starting: (self start) ending: end. 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> translateTo: aTimeZone [
	"Keep myself's representation and move it to another timezone offset.
	Note that unlike #offset: this WILL change the absolute time in UTC"
	
	^ self class
		starting: (start translateTo: aTimeZone)
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> translateToUTC [ 
	" Move this represenation to UTC"
	^ self translateTo: 0
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> union: aTimespan [
	 "Return the Timespan spanned by both"

	| aBegin anEnd |

	aBegin := self start min: aTimespan start.
	anEnd := self end max: aTimespan end.
	^ Timespan starting: aBegin ending: (anEnd + DateAndTime clockPrecision).

]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> weeks [


	| weeks |
	weeks := OrderedCollection new.
	self weeksDo: [ :m | weeks add: m ].
	^ weeks asArray.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> weeksDo: aBlock [

	self do: aBlock with: self asWeek.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> workDatesDo: aBlock [
 	"Exclude Saturday and Sunday"

	self do: aBlock with: start asDate when: [ :d | d dayOfWeek < 6 ]. 
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> years [


	| years |
	years := OrderedCollection new.
	self yearsDo: [ :m | years add: m ].
	^ years asArray.
]

{ #category : #'*Kernel-Chronology-Extras' }
Timespan >> yearsDo: aBlock [

	self do: aBlock with: start asYear.
]
