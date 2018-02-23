"
Instances of Date are Timespans with duration of 1 day.
Their default creation assumes a start of midnight in the local time zone.

Comparing Dates

We tend to use dates in one of two modes:

- Time zone dependent
- Time zone independent

In the first instance, dates are only the same if they are in the same
time zone (otherwise they are two different 24 hour periods).  This is
the default behaviour of Date.

In the second, we are only interested in whether the day, month
and year are the same.

As an example, take someone's birthday.  If I want to know whether we
were born on the same day (and the same age), I will want to compare dates without time zones.  If I want to know if it is currently their birthday where they
are, I'll want to use time zones.

To compare two dates with time zones use #<, #<=, #=, #>= and #>.

To compare two dates ignoring time zones use #isBefore:, #isOnOrBefore:, #equals:, #isOnOrAfter: and #isAfter:.

| birthday1 birthday2 |

birthday1 := (DateAndTime fromString: '2018/01/01T00:00:00+10') asDate.
birthday2 := (DateAndTime fromString: '2018/01/01T00:00:00+01') asDate.

""Do person 1 and person 2 have the same birthday?""
birthday1 equals: birthday2.   ""true""

""Do birthday1 and birthday2 represent the same 24 hour period of time?""
birthday1 = birthday2.  ""false""

birthday1 < birthday2.  ""true""
birthday1 isBefore: birthday2.  ""false""
birthday1 isOnOrBefore: birthday2.  ""true""

"
Class {
	#name : #Date,
	#superclass : #Timespan,
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Chronology'
}

{ #category : #'accessing constant' }
Date class >> dayNames [
	"Return a collection of english day names"
	^ DayNames copy
]

{ #category : #accessing }
Date class >> dayOfWeek: dayName [ 

	^ Week indexOfDay: dayName
]

{ #category : #'accessing constant' }
Date class >> daysInMonth [
	"Return a collection representing the days in each month"
	^ DaysInMonth copy
]

{ #category : #accessing }
Date class >> daysInMonth: monthName forYear: yearInteger [ 

	^ Month daysInMonth: monthName forYear: yearInteger.

]

{ #category : #accessing }
Date class >> daysInYear: yearInteger [ 

	^ Year daysInYear: yearInteger.
]

{ #category : #'specific inquiries' }
Date class >> easterDateFor: year [

 "  compute the easter date.
    source: Physikalisch-Technische Bundesanstalt Braunschweig.
    Lichtenberg, H.: Zur Interpretation der Gaussschen Osterformel
                     und ihrer Ausnahmeregeln,
                     Historia Mathematica 24 (1997), pp. 441-444
  
    http://www.ptb.de/de/org/4/44/441/oste.htm
  "

  | k m s a d r og sz oe day |

  k := year // 100.
  m := 15 + (3*k + 3//4) - (8*k + 13//25).
   s := 2 - (3*k + 3// 4).
  a := year \\ 19.
  d := 19*a + m \\ 30.
  r := d//29 + ((d//28) - (d//29)* (a// 11)).

  og := 21 + d - r.
  sz := 7 - (year//4 + year + s\\7).
  oe := 7 - (og - sz\\7).
  day := og + oe.
  ^day <= 31
    ifTrue: [ Date year: year month: 3 day: day ]
    ifFalse: [ Date year: year month: 4 day: day - 31 ].
]

{ #category : #accessing }
Date class >> firstWeekdayOfMonth: month year: year [
	"Answer the weekday index of the first day in <month> in the <year>."

	^ (self year: year month: month day: 1) weekdayIndex

]

{ #category : #'instance creation' }
Date class >> fromDays: dayCount [ 
	"Days since 1 January 1901"

	^ self julianDayNumber: SqueakEpoch + dayCount
]

{ #category : #'instance creation' }
Date class >> fromSeconds: seconds [
	"Answer an instance of me which is 'seconds' seconds after January 1, 1901."

	^ self starting: (DateAndTime fromSeconds: seconds)
]

{ #category : #importing }
Date class >> fromString: aString [
	"Answer an instance of created from a string with format mm.dd.yyyy or mm-dd-yyyy or mm/dd/yyyy"

	^ self readFrom: aString readStream.
]

{ #category : #accessing }
Date class >> indexOfMonth: aMonthName [ 

	^ Month indexOfMonth: aMonthName.

]

{ #category : #'instance creation' }
Date class >> julianDayNumber: aJulianDayNumber [

	^ self starting: (DateAndTime julianDayNumber: aJulianDayNumber)
]

{ #category : #'instance creation' }
Date class >> julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset [

	^ self starting: (DateAndTime julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset)
]

{ #category : #'accessing constant' }
Date class >> monthNames [
	"Return a collection of english month names"
	^ MonthNames copy
]

{ #category : #accessing }
Date class >> nameOfDay: dayIndex [ 

	^ Week nameOfDay: dayIndex 
]

{ #category : #accessing }
Date class >> nameOfMonth: anIndex [ 

	^ Month nameOfMonth: anIndex.

]

{ #category : #'instance creation' }
Date class >> newDay: day month: month year: year [ 

	^ self year: year month: month day: day
]

{ #category : #'instance creation' }
Date class >> newDay: dayCount year: yearInteger [

	^ self year: yearInteger day: dayCount
]

{ #category : #'specific inquiries' }
Date class >> orthodoxEasterDateFor: year [

 "  compute the easter date according to the rules of the orthodox calendar.
    source: 
    http://www.smart.net/~mmontes/ortheast.html 
  "
     | r1 r2 r3 r4 ra rb r5 rc date |

    r1 := year \\ 19.
    r2 := year \\ 4.
    r3 := year \\ 7.
    ra := 19*r1 + 16.
    r4 := ra \\ 30.
    rb := r2 + r2 + (4*r3) + (6*r4).
    r5 := rb \\ 7.
    rc := r4 + r5.
    date := Date year: year month: 4 day: 3.
    ^date addDays: rc.
]

{ #category : #importing }
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

{ #category : #importing }
Date class >> readFrom: aStringOrStream pattern: pattern [
	"See 
	DateParser comment"

	^ (DateParser readingFrom: aStringOrStream readStream pattern: pattern) parse

]

{ #category : #'instance creation' }
Date class >> starting: aDateAndTime [

	^ super starting: (aDateAndTime midnight) duration: (Duration days: 1)

]

{ #category : #'instance creation' }
Date class >> today [

	^ self current
]

{ #category : #'instance creation' }
Date class >> tomorrow [

	^ self today next
]

{ #category : #'instance creation' }
Date class >> week: week day: dayOfWeek [

	^ self starting: (Week week: week) start + (dayOfWeek - 1) days
]

{ #category : #'instance creation' }
Date class >> year: year day: dayOfYear [

	^ self starting: (DateAndTime year: year day: dayOfYear)

]

{ #category : #'instance creation' }
Date class >> year: year month: month day: day [

	^ self starting: (DateAndTime year: year month: month day: day)

]

{ #category : #'instance creation' }
Date class >> year: year week: week day: dayOfWeek [

	^ self starting: (Week year: year week: week) start + (dayOfWeek - 1) days
]

{ #category : #'instance creation' }
Date class >> yesterday [

	^ self today previous
]

{ #category : #adding }
Date >> addDays: dayCount [ 

	^ (self asDateAndTime + (dayCount days)) asDate
]

{ #category : #utilities }
Date >> addMonths: monthCount [ 
	|year month maxDaysInMonth day |
	year := self year + (monthCount + self monthIndex - 1 // 12).
	month := self monthIndex + monthCount - 1 \\ 12 + 1.
	maxDaysInMonth := Month daysInMonth: month forYear: year.
	day := self dayOfMonth min: maxDaysInMonth.
	^ Date year: year month: month day: day
]

{ #category : #accessing }
Date >> asDate [

	^ self
]

{ #category : #enumerating }
Date >> dayMonthYearDo: aBlock [ 
	"Supply integers for day, month and year to aBlock and return the result"

	^ start dayMonthYearDo: aBlock
]

{ #category : #printing }
Date >> ddmmyyyy [
	"Print the receiver  in standard French format dd/mm/yyyy."

	^ self printFormat: #(1 2 3 $/ 1 1)

]

{ #category : #'comparing - ignore timezone' }
Date >> equals: aDate [
    "Perform a time zone independent comparison of the dates, i.e. only compare day, month and year.
	To compare with time zones, use #="

    ^self year = aDate year and: [
        self monthIndex = aDate monthIndex and:
        [ self dayOfMonth = aDate dayOfMonth ] ]
]

{ #category : #'comparing - ignore timezone' }
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

{ #category : #'comparing - ignore timezone' }
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

{ #category : #'comparing - ignore timezone' }
Date >> isOnOrAfter: aDate [
	"Answer a boolean indicating whether the receiver is on or later than aDate (ignoring time zones).
	To compare with time zones, use #>="

	^(self equals: aDate) or: [ self isAfter: aDate ]
]

{ #category : #'comparing - ignore timezone' }
Date >> isOnOrBefore: aDate [
	"Answer a boolean indicating whether the receiver is on or earlier than aDate (ignoring time zones).
	To compare with time zones, use #<="

	^(self equals: aDate) or: [ self isBefore: aDate ]
]

{ #category : #printing }
Date >> mmddyyyy [
	"Print the receiver in standard U.S.A format mm/dd/yyyy.
	Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, 
	so that for example February 1 1996 is 2/1/96"

	^ self printFormat: #(2 1 3 $/ 1 1)
]

{ #category : #accessing }
Date >> month [
	^ self asMonth
]

{ #category : #accessing }
Date >> monthIndex [
	^ super month
]

{ #category : #utilities }
Date >> onNextMonth [

	^ self addMonths: 1

]

{ #category : #utilities }
Date >> onPreviousMonth [

	^ self addMonths: -1

]

{ #category : #accessing }
Date >> previous: dayName [ 
	"Answer the previous date whose weekday name is dayName."

	| days |
	days := 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
	days = 0 ifTrue: [ days := 7 ].
	^ self subtractDays: days

]

{ #category : #printing }
Date >> printFormat: formatArray [ 
	"Answer a String describing the receiver using the argument formatArray."

	^ String new: 16 streamContents: [ :aStream |
		self printOn: aStream format: formatArray ]
]

{ #category : #printing }
Date >> printOn: aStream [
 
 	self printOn: aStream format: #(1 2 3 $  3 1 )
]

{ #category : #printing }
Date >> printOn: aStream format: formatArray [ 
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| day month year twoDigits element monthFormat |
	self dayMonthYearDo: [ :d :m :y | day := d. month := m. year := y ].
	twoDigits := formatArray size > 6 and: [ (formatArray at: 7) > 1 ].
	1 to: 3 do:  [ :i | 
		element := formatArray at: i.
		element = 1
			ifTrue: [
				twoDigits
					ifTrue: [ day printOn: aStream base: 10 length: 2 padded: true ]
					ifFalse: [ day printOn: aStream ] ].
		element = 2
			ifTrue: [
				monthFormat := formatArray at: 5.
				monthFormat = 1
					ifTrue: [
						twoDigits
							ifTrue: [ month printOn: aStream base: 10 length: 2 padded: true ]
							ifFalse: [ month printOn: aStream]].
				monthFormat = 2
					ifTrue: [ (Month nameOfMonth: month) from: 1 to: 3 do: [ :each | aStream nextPut: each ] ].
				monthFormat = 3
					ifTrue: [ aStream nextPutAll: (Month nameOfMonth: month) ] ].
		element = 3
			ifTrue: [
				(formatArray at: 6) = 1
					ifTrue: [ year printOn: aStream base: 10 length: 4 padded: true ]
					ifFalse: [ (year \\ 100) printOn: aStream base: 10 length: 2 padded: true ] ].
		i < 3
			ifTrue: [
				(formatArray at: 4) ~= 0
					ifTrue: [ aStream nextPut: (formatArray at: 4) asCharacter ] ] ]
]

{ #category : #printing }
Date >> storeOn: aStream [
 
 	aStream print: self printString; nextPutAll: ' asDate'
]

{ #category : #adding }
Date >> subtractDate: aDate [ 
	"Answer the number of days between self and aDate"

	^ (self start - aDate asDateAndTime) days
]

{ #category : #adding }
Date >> subtractDays: dayCount [ 

	^ (self asDateAndTime - (dayCount days)) asDate
]

{ #category : #accessing }
Date >> weekday [
	"Answer the name of the day of the week on which the receiver falls."

	^ self dayOfWeekName
]

{ #category : #accessing }
Date >> weekdayIndex [
	"Sunday=1, ... , Saturday=7"

	^ self dayOfWeek
]

{ #category : #printing }
Date >> yyyymmdd [
 	"Format the date in ISO 8601 standard like '2002-10-22'
	The result is of fixed size 10 characters long.."
 
 	^ String new: 10 streamContents: [ :aStream |
		self printOn: aStream format: #(3 2 1 $- 1 1 2) ]
]
