Extension { #name : #Date }

{ #category : #'*Kernel-Chronology-Extras' }
Date >> addDays: dayCount [ 
	"((Date year: 2018 month: 9 day: 28) addDays: 3) printString >>> '1 October 2018'"

	^ (self asDateAndTime + (dayCount days)) asDate
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> addMonths: monthCount [ 
	"((Date year: 2018 month: 9 day: 28) addMonths: 3) printString>>> '28 December 2018'"
	
	|year month maxDaysInMonth day |
	year := self year + (monthCount + self monthIndex - 1 // 12).
	month := self monthIndex + monthCount - 1 \\ 12 + 1.
	maxDaysInMonth := Month daysInMonth: month forYear: year.
	day := self dayOfMonth min: maxDaysInMonth.
	^ Date year: year month: month day: day
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> asMonth: aMonthNumber inYear: aYearNumber [
      "Returns the month corresponding the month number"
    
        ^ Month year: aYearNumber month: aMonthNumber
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> dayNames [
	"Return a collection of english day names"
	^ DayNames copy
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> dayOfWeek: dayName [ 

	^ Week indexOfDay: dayName
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> daysInMonth [
	"Return a collection representing the days in each month"
	^ DaysInMonth copy
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> daysInMonth: monthName forYear: yearInteger [ 

	^ Month daysInMonth: monthName forYear: yearInteger.

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> daysInYear: yearInteger [ 

	^ Year daysInYear: yearInteger.
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> ddmmyyyy [
	"Print the receiver  in standard French format dd/mm/yyyy."
	"(Date year: 2018 month: 9 day: 28) ddmmyyyy >>> '28/9/2018'"

	^ self printFormat: #(1 2 3 $/ 1 1)

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> firstWeekdayOfMonth: month year: year [
	"Answer the weekday index of the first day in <month> in the <year>."

	^ (self year: year month: month day: 1) weekdayIndex

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> fromString: aString [
	"Answer an instance of created from a string with format mm.dd.yyyy or mm-dd-yyyy or mm/dd/yyyy"

	^ self readFrom: aString readStream.
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> indexOfMonth: aMonthName [ 

	^ Month indexOfMonth: aMonthName.

]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> isAfter: aDate [
	"Answer a boolean indicating whether the receiver is later than aDate (ignoring time zones).
	To compare with time zones, use #>"

	| otherDate |

	otherDate := aDate asDate.
	^self year > otherDate year or: [ 
		self year = otherDate year and: [ 
			self monthIndex > otherDate monthIndex or: [ 
				self monthIndex = otherDate monthIndex and: [ 
					self dayOfMonth > otherDate dayOfMonth ] ] ] ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> isBefore: aDate [
	"Answer a boolean indicating whether the receiver is earlier than aDate (ignoring time zones).
	To compare with time zones, use #<"

	| otherDate |

	otherDate := aDate asDate.
	^self year < otherDate year or: [ 
		self year = otherDate year and: [ 
			self monthIndex < otherDate monthIndex or: [ 
				self monthIndex = otherDate monthIndex and: [ 
					self dayOfMonth < otherDate dayOfMonth ] ] ] ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> isOnOrAfter: aDate [
	"Answer a boolean indicating whether the receiver is on or later than aDate (ignoring time zones).
	To compare with time zones, use #>="

	^(self equals: aDate) or: [ self isAfter: aDate ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> isOnOrBefore: aDate [
	"Answer a boolean indicating whether the receiver is on or earlier than aDate (ignoring time zones).
	To compare with time zones, use #<="

	^(self equals: aDate) or: [ self isBefore: aDate ]
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> julianDayNumber: aJulianDayNumber [

	^ self starting: (DateAndTime julianDayNumber: aJulianDayNumber)
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset [

	^ self starting: (DateAndTime julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset)
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> mmddyyyy [
	"Print the receiver in standard U.S.A format mm/dd/yyyy.
	Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros,
	so that for example February 1 1996 is 2/1/96"
	"(Date year: 2018 month: 9 day: 28) mmddyyyy >>> '9/28/2018'"

	^ self printFormat: #(2 1 3 $/ 1 1)
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> month [
	"(Date year: 2018 month: 9 day: 28) month printString >>> 'September 2018'"

	^ self asMonth
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> monthIndex [
	"(Date year: 2018 month: 9 day: 28) monthIndex >>> 9"
		
	^ super month
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> monthNames [
	"Return a collection of english month names"
	^ MonthNames copy
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> nameOfDay: dayIndex [ 

	^ Week nameOfDay: dayIndex 
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> nameOfMonth: anIndex [ 

	^ Month nameOfMonth: anIndex.

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> newDay: day month: month year: year [ 

	^ self year: year month: month day: day
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> newDay: dayCount year: yearInteger [

	^ self year: yearInteger day: dayCount
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> onNextMonth [
	"(Date year: 2018 month: 9 day: 28) onNextMonth printString >>> '28 October 2018'"

	^ self addMonths: 1

]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> onPreviousMonth [
	"(Date year: 2018 month: 9 day: 28) onPreviousMonth printString >>> '28 August 2018'"

	^ self addMonths: -1

]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> previous: dayName [ 
	"Answer the previous date whose weekday name is dayName."
	"((Date year: 2018 month: 9 day: 28) previous: 'Sunday') printString >>> '23 September 2018'"

	| days |
	days := 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
	days = 0 ifTrue: [ days := 7 ].
	^ self subtractDays: days

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> readFrom: aStream [ 
	"Read a Date from the stream in any of the forms:  
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>			(1982-04-15) (ISO8601)"
	| day month year parsedNumber prefix |
	aStream peek = $-
		ifTrue: [prefix := -1]
		ifFalse: [prefix := 1].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	aStream peek isDigit
		ifTrue: [
			parsedNumber := (Integer readFrom: aStream) * prefix.
			(parsedNumber < 0 or: [parsedNumber > 31])
				ifTrue: [year := parsedNumber]].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			month := (String new: 10) writeStream.
			[aStream peek isLetter]
				whileTrue: [month nextPut: aStream next].
			month := month contents.
			[aStream peek isAlphaNumeric]
				whileFalse: [aStream skip: 1].
			parsedNumber 
				ifNil: ["MM DD YY"
					day := Integer readFrom: aStream]
				ifNotNil: [
					year 
						ifNil: ["DD MM YY"
							day := parsedNumber]]]
		ifFalse: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			year 
				ifNil: ["MM-DD-YY or DD-MM-YY"
					parsedNumber > 12
						ifTrue: ["DD-MM-YY"
							Error signal: 'Month out of bounds: ', parsedNumber asString, '.'.
							day := parsedNumber.
							month := Month nameOfMonth: (Integer readFrom: aStream) ]
						ifFalse: ["MM-DD-YY"
							month := Month nameOfMonth: parsedNumber.
							day := Integer readFrom: aStream]]
				ifNotNil: ["YY-MM-DD"
					month := Month nameOfMonth: (Integer readFrom: aStream)]].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	year 
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0]) 
		ifTrue: [
			year < 69 
				ifTrue: [year := 2000 + year]
				ifFalse: [year := 1900 + year]].

	^ self
		year: year
		month: month
		day: day

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> readFrom: aStringOrStream pattern: pattern [
	"See 
	DateParser comment"

	^ (DateParser readingFrom: aStringOrStream readStream pattern: pattern) parse

]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> storeOn: aStream [
 
 	aStream print: self printString; nextPutAll: ' asDate'
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> subtractDate: aDate [ 
	"Answer the number of days between self and aDate"
	"((Date year: 2018 month: 9 day: 28) subtractDate: '27 September 2018') >>> 1"

	^ (self start - aDate asDateAndTime) days
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> subtractDays: dayCount [ 
	"((Date year: 2018 month: 9 day: 28) subtractDays: 1) printString >>> '27 September 2018'"

	^ (self asDateAndTime - (dayCount days)) asDate
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> tomorrow [

	^ self today next
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> week: week day: dayOfWeek [

	^ self starting: (Week week: week) start + (dayOfWeek - 1) days
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> weekday [
	"Answer the name of the day of the week on which the receiver falls."
	"(Date year: 2018 month: 9 day: 28) weekday >>> #Friday"

	^ self dayOfWeekName
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> weekdayIndex [
	"Sunday=1, ... , Saturday=7"
	"(Date year: 2018 month: 9 day: 28) weekdayIndex >>> 6"
	
	^ self dayOfWeek
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> year: year day: dayOfYear [

	^ self starting: (DateAndTime year: year day: dayOfYear)

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> year: year month: month day: day [

	^ self starting: (DateAndTime year: year month: month day: day)

]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> year: year week: week day: dayOfWeek [

	^ self starting: (Week year: year week: week) start + (dayOfWeek - 1) days
]

{ #category : #'*Kernel-Chronology-Extras' }
Date class >> yesterday [

	^ self today previous
]

{ #category : #'*Kernel-Chronology-Extras' }
Date >> yyyymmdd [
 	"Format the date in ISO 8601 standard like '2002-10-22'
	The result is of fixed size 10 characters long.."
 	"(Date year: 2018 month: 9 day: 28) yyyymmdd >>> '2018-09-28'"
	
 	^ String new: 10 streamContents: [ :aStream |
		self printOn: aStream format: #(3 2 1 $- 1 1 2) ]
	
	
]
