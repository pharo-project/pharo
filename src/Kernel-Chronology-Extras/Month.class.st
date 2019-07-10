"
I represent a month.
"
Class {
	#name : #Month,
	#superclass : #Timespan,
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #'instance creation' }
Month class >> april [
    ^ self month: 4
]

{ #category : #'instance creation' }
Month class >> august [
    ^ self month: 8
]

{ #category : #'instance creation' }
Month class >> current [
	^ self starting: DateAndTime now
]

{ #category : #accessing }
Month class >> daysInMonth: indexOrName forYear: yearInteger [ 

	| index |
	index := indexOrName isInteger 
				ifTrue: [indexOrName]
				ifFalse: [self indexOfMonth: indexOrName].
	^ (DaysInMonth at: index)
			+ ((index = 2
					and: [Year isLeapYear: yearInteger])
						ifTrue: [1] ifFalse: [0])
]

{ #category : #'instance creation' }
Month class >> december [
    ^ self month: 12
]

{ #category : #'instance creation' }
Month class >> february [
    ^ self month: 2
]

{ #category : #accessing }
Month class >> indexOfMonth: aMonthName [ 

	1 to: 12 do: [ :i |  (aMonthName, '*' match: (MonthNames at: i)) ifTrue: [^i] ].
 	self error: aMonthName , ' is not a recognized month name'.
]

{ #category : #'instance creation' }
Month class >> january [
    ^ self month: 1
]

{ #category : #'instance creation' }
Month class >> july [
    ^ self month: 7
]

{ #category : #'instance creation' }
Month class >> june [
    ^ self month: 6
]

{ #category : #'instance creation' }
Month class >> march [
    ^ self month: 3
]

{ #category : #'instance creation' }
Month class >> may [
    ^ self month: 5
]

{ #category : #'instance creation' }
Month class >> month: month [
	"Create a Month in the current year for the given <month>.
	<month> may be a number or a String with the name of the month."

	^ self 
		year: DateAndTime now year
		month: month

]

{ #category : #'instance creation' }
Month class >> month: month year: year [
	^ self year: year month: month
]

{ #category : #accessing }
Month class >> nameOfMonth: anIndex [

	^ MonthNames at: anIndex.
]

{ #category : #'instance creation' }
Month class >> november [
    ^ self month: 11
]

{ #category : #'instance creation' }
Month class >> october [
    ^ self month: 10
]

{ #category : #'instance creation' }
Month class >> readFrom: aStream [

	| m y c |
	m := (ReadWriteStream with: '') reset.
	[(c := aStream next) isSeparator] whileFalse: [m nextPut: c].
	[(c := aStream next) isSeparator] whileTrue.
	y := (ReadWriteStream with: '') reset.
	y nextPut: c.
	[aStream atEnd] whileFalse: [y nextPut: aStream next].

	^ self 
		year: y contents asNumber
		month: (Month indexOfMonth: m contents)

"Month readFrom: 'July 1998' readStream"

]

{ #category : #'instance creation' }
Month class >> september [
    ^ self month: 9
]

{ #category : #'instance creation' }
Month class >> starting: aDateAndTime duration: aDuration [ 
	"Override - a each month has a defined duration"
	| start adjusted days |
	start := aDateAndTime asDateAndTime.
	adjusted := DateAndTime
				year: start year
				month: start month
				day: 1
				offset: aDateAndTime offset.
	days := self 
		daysInMonth: adjusted month 
		forYear: adjusted year.
	^ super
		starting: adjusted
		duration: (Duration days: days)
]

{ #category : #'instance creation' }
Month class >> year: year month: month [
	"Create a Month for the given <year> and <month>.
	<month> may be a number or a String with the
	name of the month. <year> should be with 4 digits."

	^ self starting: (DateAndTime year: year month: month day: 1)

]

{ #category : #conversion }
Month >> asMonth [

	^ self 
]

{ #category : #accessing }
Month >> daysInMonth [

	^ self duration days.
]

{ #category : #accessing }
Month >> index [

	^ self monthIndex 
]

{ #category : #testing }
Month >> name [ 

	^ self monthName 
]

{ #category : #accessing }
Month >> previous [ 

	^ self class starting: (self start - 1) 
]

{ #category : #printing }
Month >> printOn: aStream [ 

	aStream nextPutAll: self monthName, ' ', self year printString.
]

{ #category : #private }
Month >> species [

	^Timespan
]
