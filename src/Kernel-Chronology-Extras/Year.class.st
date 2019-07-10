"
I represent a year.
"
Class {
	#name : #Year,
	#superclass : #Timespan,
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #'instance creation' }
Year class >> currentYear [
  "Return the current year"
  ^ Date today asYear
]

{ #category : #'instance creation' }
Year class >> currentYearNumber [
    "Return the current year number"
    ^ Date today year
]

{ #category : #accessing }
Year class >> daysInYear: yearInteger [

	^ 365 + ((self isLeapYear: yearInteger) ifTrue: [1] ifFalse: [0]).

]

{ #category : #accessing }
Year class >> isLeapYear: aYearInteger [


	| adjustedYear |
	adjustedYear := aYearInteger > 0
		ifTrue: [aYearInteger]
		ifFalse: [(aYearInteger + 1) negated].

	"There was no year 0"
	^ ((adjustedYear \\ 4 ~= 0) or: [(adjustedYear \\ 100 = 0) and: [adjustedYear \\ 400 ~= 0]]) not.
]

{ #category : #'instance creation' }
Year class >> starting: aDateAndTime duration: aDuration [ 
	"Return a new year, starting from the beginning the year the given DateAndTime is in."
	| aYear |
	aYear := aDateAndTime asDateAndTime year.

	^ super
		starting: (DateAndTime year: aYear month: 1 day: 1 offset: aDateAndTime offset)
		duration: (Duration days: (self daysInYear: aYear)).
]

{ #category : #'instance creation' }
Year class >> year: aYear [

	^ self starting: (DateAndTime year: aYear month: 1 day: 1).
]

{ #category : #conversion }
Year >> asYear [ 

	^ self 
]

{ #category : #accessing }
Year >> daysInMonth [ 

	^ Date daysInMonth  
]

{ #category : #accessing }
Year >> daysInMonth: aMonthNumber [ 
    "Returns the days in month for a given month in the context of the receiver"

    ^ (DateAndTime year: self year month: aMonthNumber day:1) asMonth daysInMonth
]

{ #category : #accessing }
Year >> daysInYear [

	^ self duration days.
]

{ #category : #accessing }
Year >> firstThursday [
	"Return the year's first Thursday.
	According to the ISO 8601 definition, the first week starts with the year's first Thursday (week day number 5)"
	^ self start + ((5 - self start dayOfWeek + 7) rem: 7) days
]

{ #category : #changes }
Year >> previous [
	"This implementation handles leap years correctly"
	
	^ self class year: (self year - 1)
]

{ #category : #printing }
Year >> printOn: aStream [

	aStream nextPutAll: 'a Year ('.
 	self start year printOn: aStream. 
	aStream nextPutAll: ')'.

]

{ #category : #private }
Year >> species [

	^Timespan
]
