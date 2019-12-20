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

{ #category : #'instance creation' }
Date class >> starting: aDateAndTime [

	^ super starting: (aDateAndTime midnight) duration: (Duration days: 1)

]

{ #category : #'instance creation' }
Date class >> today [

	^ self current
]

{ #category : #accessing }
Date >> asDate [
	"(Date year: 2018 month: 9 day: 28) asDate printString >>> '28 September 2018'"

	^ self
]

{ #category : #accessing }
Date >> dayMonthYearDo: aBlock [ 
	"Supply integers for day, month and year to aBlock and return the result"

	^ start dayMonthYearDo: aBlock
]

{ #category : #'comparing - ignore timezone' }
Date >> equals: aDate [
	"Perform a time zone independent comparison of the dates, i.e. only compare day, month and year.
	To compare with time zones, use #="
	"(Date today equals: Date yesterday) >>> false"

    ^self year = aDate year and: [
        self monthIndex = aDate monthIndex and:
        [ self dayOfMonth = aDate dayOfMonth ] ]
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
	
	BasicDatePrinter default printDate: self format: formatArray on: aStream
]

{ #category : #private }
Date >> species [

	^Timespan
]
