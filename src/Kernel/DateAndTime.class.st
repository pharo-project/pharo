"
I am DateAndTime.
I represent a point in time or timestamp as defined by ISO 8601. 
I am a Magnitude. 
I have nanosecond precision.
I am TimeZone aware. 
I have zero duration.

  DateAndTime now.
  DateAndTime now asUTC rounded.
  DateAndTime fromString: '1969-07-20T20:17:40.123+02:00'.
  DateAndTime fromString: '1969-07-20T20:17:40Z'.

My implementation uses three SmallIntegers and a Duration:
  julianDayNumber - julian day number (starting at midnight UTC rather than noon GMT).
  seconds - number of seconds since midnight UTC. Always positive, between 0 and 86399.
  nanos	 - the number of nanoseconds since the second. Always positive, between 0 and 999999999.
  offset	- duration from UTC.

The offset is used to print the date and time in a local time zone, but the date and time are handled in UTC internally.
The nanosecond attribute is often zero but it defined for full ISO compliance and is suitable for timestamping.
"
Class {
	#name : #DateAndTime,
	#superclass : #Magnitude,
	#instVars : [
		'seconds',
		'offset',
		'julianDayNumber',
		'nanos'
	],
	#classVars : [
		'ClockProvider',
		'LocalTimeZone'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : #Kernel-Chronology
}

{ #category : #'clock provider' }
DateAndTime class >> clock [ 
	 "the provider of real time seconds/milliseconds."

	^ ClockProvider
]

{ #category : #'system queries' }
DateAndTime class >> clockPrecision [
	"One nanosecond precision"

	^ Duration seconds: 0 nanoSeconds: 1
]

{ #category : #'instance creation queries' }
DateAndTime class >> current [

	^ self now
]

{ #category : #'instance creation' }
DateAndTime class >> date: aDate time: aTime [

	^ self 
		year: aDate year
		month: aDate monthName
		day: aDate dayOfMonth 
		hour: aTime hour 
		minute: aTime minute 
		second: aTime second
		nanoSecond: aTime nanoSecond
		offset: self localOffset.
]

{ #category : #'instance creation queries' }
DateAndTime class >> dosEpoch [
	"Answer a DateAndTime representing the DOS epoch (1 January 1980, midnight UTC)"

	^ self basicNew
		ticks: #(2444240 0 0) offset: Duration zero;
		yourself.
]

{ #category : #'instance creation queries' }
DateAndTime class >> epoch [
  "Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

  ^ (self julianDayNumber: SqueakEpoch) offset: 0.
]

{ #category : #'instance creation' }
DateAndTime class >> fromDosTimestamp: anInteger [

	^ (DosTimestamp on: anInteger) asDateAndTime.
]

{ #category : #'instance creation' }
DateAndTime class >> fromMethodTimeStamp: aString [
	| stream |
	stream := aString readStream.
	stream
		skipSeparators;
		skipTo: Character space.
	^ self readSeparateDateAndTimeFrom: stream
]

{ #category : #'instance creation' }
DateAndTime class >> fromSeconds: secondsSinceEpochUTC [
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the seconds in UTC time"
	^ self fromSeconds: secondsSinceEpochUTC offset: self localOffset
]

{ #category : #'instance creation' }
DateAndTime class >> fromSeconds: utcSecondsSinceEpoch offset: aUTCOffset [
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the given timeZone"

	| integerSeconds nanos |
	integerSeconds := utcSecondsSinceEpoch truncated.
	nanos := integerSeconds = utcSecondsSinceEpoch
		ifTrue: [ 0 ]
		ifFalse: [ ((utcSecondsSinceEpoch - integerSeconds) * NanosInSecond) asInteger ].
	^ self basicNew ticks: {SqueakEpoch . integerSeconds . nanos} offset: aUTCOffset asDuration
]

{ #category : #input }
DateAndTime class >> fromString: aString [ 
	^ self readFrom: aString readStream
]

{ #category : #'instance creation' }
DateAndTime class >> fromUnixTime: anInteger [
	^ self fromSeconds: anInteger + 2177452800 "unix epoch constant"
]

{ #category : #input }
DateAndTime class >> fuzzyReadFrom: aStream [
	| bc year month day hour minute second nanos offset buffer ch |


	aStream peek = $- ifTrue: [ aStream next. bc := -1] ifFalse: [bc := 1].
	year := (aStream upTo: $-) asInteger * bc.
	month := (aStream upTo: $-) asInteger ifNil: [1].
	day := (aStream upTo: $T) asInteger ifNil: [1].
	hour := (aStream upTo: $:) asInteger ifNil: [0].
 	buffer := '00:' copy. ch := nil.
	minute := buffer writeStream.
	[ aStream atEnd | (ch = $:) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch := minute nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch := $: ].
	minute := (buffer readStream upTo: ch) asInteger.
	buffer := '00.' copy.
	second := buffer writeStream.
	[ aStream atEnd | (ch = $.) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch := second nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch := $. ].
	second := (buffer readStream upTo: ch) asInteger.
	buffer := '000000000' copy.
	(ch = $.) ifTrue: [ 
		nanos := buffer writeStream.
		[ aStream atEnd | ((ch := aStream next) = $+) | (ch = $-) ]
			whileFalse: [ nanos nextPut: ch. ].
		(ch isNil or: [ch isDigit]) ifTrue: [ ch := $+ ].
	].

	nanos := buffer asInteger.
	aStream atEnd
		ifTrue: [ offset := Duration zero ]
		ifFalse: [ch := aStream next.
                       ch = $+ ifTrue: [ch := Character space].
                       offset := Duration fromString: ch asString, '0:', aStream upToEnd, ':0'].
	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute

		second: second
		nanoSecond:  nanos

		offset: offset.
]

{ #category : #initialize-release }
DateAndTime class >> initialize [
	ClockProvider := Time
]

{ #category : #'instance creation' }
DateAndTime class >> julianDayNumber: aJulianDayNumber [

	^ self basicNew
		ticks: aJulianDayNumber days ticks offset: Duration new;
		yourself
]

{ #category : #'instance creation' }
DateAndTime class >> julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset [
	"Return a DateAndTime at midnight local time at the given julian day"
	| ticks |
	"create a ticks representation in UTC, take the given julian day in local time"
	ticks := aJulianDayNumber days ticks.
	ticks at: 2 put: aTimeZoneOffset asSeconds negated.

	^ self basicNew
		ticks: ticks offset: aTimeZoneOffset;
		yourself
]

{ #category : #'system queries' }
DateAndTime class >> localOffset [
	"Answer the duration we are offset from UTC"

	^ self localTimeZone offset
]

{ #category : #'time zones' }
DateAndTime class >> localTimeZone [
	"Answer the local time zone"

	^ LocalTimeZone ifNil: [ LocalTimeZone := TimeZone default ]

]

{ #category : #'time zones' }
DateAndTime class >> localTimeZone: aTimeZone [
	"Set the local time zone"

	"
	DateAndTime localTimeZone: (TimeZone offset:  0 hours name: 'Universal Time' abbreviation: 'UTC').
	DateAndTime localTimeZone: (TimeZone offset: -8 hours name: 'Pacific Standard Time' abbreviation: 'PST').
	"

	LocalTimeZone := aTimeZone


]

{ #category : #'instance creation queries' }
DateAndTime class >> midnight [

	^ self now midnight
]

{ #category : #primitives }
DateAndTime class >> millisecondClockValue [

	^ self clock millisecondClockValue
]

{ #category : #'instance creation' }
DateAndTime class >> new [
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self epoch offset: self localOffset 
]

{ #category : #'instance creation queries' }
DateAndTime class >> noon [

	^ self now noon
]

{ #category : #'instance creation' }
DateAndTime class >> now [ 
	"Answer the current date and time expressed in local time.
	[ 10000 timesRepeat: [ self now. ] ] timeToRun / 10000.0 . "

	| nanoTicks |
	nanoTicks := self clock microsecondClockValue * 1e3.
	^ self basicNew
		setJdn: SqueakEpoch 
		seconds: 0
		nano: nanoTicks
		offset: self localOffset
]

{ #category : #input }
DateAndTime class >> readFrom: aStream [
	"Parse and return a new DateAndTime instance from stream, 
	as a Date, an optional Time and an optional TimeZone offset.
	The time defaults to midnight, the timezone to the local offset"
	"self readFrom: '2013-03-04T23:47:52.876+01:00' readStream"

	^self readFrom: aStream defaultOffset: self localOffset.
]

{ #category : #input }
DateAndTime class >> readFrom: aStream defaultOffset: defaultOffset [
	"Parse and return a new DateAndTime instance from stream, 
	as a Date, an optional Time and an optional TimeZone offset.
	The time defaults to midnight, the timezone to defaultOffset"
	"self readFrom: '2013-03-04T23:47:52.876+01:00' readStream"

	| date time offset |
	date := Date readFrom: aStream.
	[ aStream atEnd or: [ '0123456789Z+-' includes: aStream peek ] ] 
		whileFalse: [ aStream next ].
  	('0123456789' includes: aStream peek)
		ifTrue: [ time := Time readFrom: aStream ]
		ifFalse: [ time := Time midnight ].
	aStream skipSeparators.
	offset := self readTimezoneOffsetFrom: aStream default: defaultOffset.
	^ self
		year: date year
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset
]

{ #category : #input }
DateAndTime class >> readOptionalSeparatorFrom: stream [
	"Read an optional separator (non decimal digit) from stream and return it.
	Return nil if nothing was read"

	| isDigit |
	stream atEnd
		ifTrue: [ ^ nil ].
	isDigit := '0123456789' includes: stream peek.
	(isDigit or: [ ':' includes: stream peek ])
		ifFalse: [ ^ nil ].
	isDigit
		ifFalse: [ stream next ]
]

{ #category : #input }
DateAndTime class >> readSeparateDateAndTimeFrom: stream [
	"Read a separate Date and Time from stream to instanciate the receiver.
	See also #printSeparateDateAndTimeOn:"
	
	| date time |
	stream skipSeparators.
	date := Date readFrom: stream.
	stream skipSeparators.
	time := Time readFrom: stream.
	^ self 
		date: date
		time: time
]

{ #category : #input }
DateAndTime class >> readTimezoneOffsetFrom: stream [
	"Read and return an optional timezone offset in the form of 
	[+|-]hh[[separator]mm[[separator]ss]] or Z from stream as a duration.
	If there is no offset, return the local offset."

	^self readTimezoneOffsetFrom: stream default: self localOffset
]

{ #category : #input }
DateAndTime class >> readTimezoneOffsetFrom: stream default: defaultOffset [
	"Read and return an optional timezone offset in the form of 
	[+|-]hh[[separator]mm[[separator]ss]] or Z from stream as a duration.
	If there is no offset, return the defaultOffset."

	| sign hour minute second |
	(stream peekFor: $Z) ifTrue: [ ^ Duration zero ].
	hour := minute := second := 0.
	^ ('+-' includes: stream peek)
		ifTrue: [
			sign := stream next = $- ifTrue: [ -1 ] ifFalse: [ 1 ].
			hour := self readTwoDigitIntegerFrom: stream.
			(self readOptionalSeparatorFrom: stream)
				ifNotNil: [ 
					minute := self readTwoDigitIntegerFrom: stream.
					(self readOptionalSeparatorFrom: stream)
						ifNotNil: [ 
							second := Integer readFrom: stream ] ].
			Duration seconds: sign * ((hour * 3600) + (minute * 60) + second) ]
		ifFalse: [ defaultOffset ]
]

{ #category : #input }
DateAndTime class >> readTwoDigitIntegerFrom: stream [
	"Parse and return a decimal number of 2 digits from stream.
	Fail if that is not possible"

	| integer |
	integer := 0.
	2 timesRepeat: [ | char |
		char := stream next.
		('0123456789' includes: char) ifFalse: [ self error: 'Decimal digit expected' ].
		integer := (integer * 10) + char digitValue ].
	^ integer
]

{ #category : #'instance creation queries' }
DateAndTime class >> today [

	^ self midnight
]

{ #category : #'instance creation queries' }
DateAndTime class >> tomorrow [

	^ self today asDate next asDateAndTime
]

{ #category : #'system queries' }
DateAndTime class >> totalSeconds [
	"Answer the total seconds ellapsed since the Squeak epoch: 1 January 1901"

	^ self clock totalSeconds
]

{ #category : #'instance creation queries' }
DateAndTime class >> unixEpoch [
	"Answer a DateAndTime representing the Unix epoch (1 January 1970, midnight UTC)"
	^ self basicNew
		ticks: #(2440588 0 0) offset: Duration zero;
		yourself.
]

{ #category : #'instance creation' }
DateAndTime class >> year: year day: dayOfYear [
	"Return a DateAndTime"

	^ self
		year: year
		day: dayOfYear
		hour: 0
		minute: 0
		second: 0
]

{ #category : #'instance creation' }
DateAndTime class >> year: year day: dayOfYear hour: hour minute: minute second: second [

	^ self
		year: year
		day: dayOfYear
		hour: hour
		minute: minute
		second: second
		offset: self localOffset.
]

{ #category : #'instance creation' }
DateAndTime class >> year: year day: dayOfYear hour: hour minute: minute second: second offset: offset [ 
	"Return a DataAndTime"

	| y d |
	y := self
		year: year
		month: 1
		day: 1
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset.
	d := Duration days: (dayOfYear - 1).
	^ y + d
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day [
	"Return a DateAndTime, midnight local time"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute [
	"Return a DateAndTime" 
	
	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute offset: anOffset [
	"Return a DateAndTime" 
	
	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0
		offset: anOffset
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second [
	"Return a DateAndTime"

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		offset: self localOffset
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second nanoSecond: nanoCount offset: utcOffset [
	"Return a DateAndTime with the values in the given TimeZone (UTCOffset)"

	| monthIndex daysInMonth p q r s julianDayNumber localSeconds utcSeconds|

	monthIndex := month isInteger ifTrue: [ month ] ifFalse: [ Month indexOfMonth: month ].
	(monthIndex between: 1 and: 12) ifFalse: [ DateError signal: 'There is no ', monthIndex printString, 'th month' ].
	daysInMonth := Month
		daysInMonth: monthIndex
		forYear: year.
	day < 1 ifTrue: [ DateError signal: 'day may not be zero or negative' ].
	day > daysInMonth ifTrue: [ DateError signal: 'day is after month ends' ].

	p := (monthIndex - 14) quo: 12.
	q := year + 4800 + p.
	r := monthIndex - 2 - (12 * p).
	s := (year + 4900 + p) quo: 100.

	julianDayNumber :=
		((1461 * q) quo: 4) +
			((367 * r) quo: 12) -
			((3 * s) quo: 4) +
			(day - 32075).
			
	localSeconds :=  hour * 60 + minute * 60 + second.
	utcSeconds := localSeconds - utcOffset asSeconds.
	
	^self basicNew
		setJdn: julianDayNumber
		seconds: utcSeconds
		nano: nanoCount
		offset: utcOffset;
		yourself
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second offset: offset [

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset
]

{ #category : #'instance creation' }
DateAndTime class >> year: year month: month day: day offset: anOffset [
	"Return a DateAndTime, midnight in the timezone with the given offset"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0
		offset: anOffset
]

{ #category : #'instance creation queries' }
DateAndTime class >> yesterday [

	^ self today asDate previous asDateAndTime
]

{ #category : #arithmetic }
DateAndTime >> + operand [
	"operand conforms to protocol Duration"

	| durationTicks |
	durationTicks := operand asDuration ticks.
	^ self class basicNew
		setJdn: julianDayNumber + durationTicks first
			seconds: seconds + durationTicks second
			nano: nanos + durationTicks third
			offset: self offset;
		yourself
]

{ #category : #arithmetic }
DateAndTime >> - operand [
	"operand conforms to protocol DateAndTime or protocol Duration"

	^ (operand respondsTo: #asDateAndTime)
		ifTrue: [ 
			| other |
			other := operand asDateAndTime.
			Duration
				seconds: (SecondsInDay * (julianDayNumber - other julianDayNumberUTC))
							+ (seconds - other secondsSinceMidnightUTC)
				nanoSeconds: nanos - other nanoSecond ]
		ifFalse: [ self + operand negated ]
]

{ #category : #arithmetic }
DateAndTime >> < comparand [
	"comparand conforms to protocol DateAndTime,
	or can be converted into something that conforms."

	| other |
	other := comparand asDateAndTime.
	^ julianDayNumber = other julianDayNumberUTC
		ifTrue: [ 
			seconds = other secondsSinceMidnightUTC
				ifTrue: [ nanos < other nanoSecond ]
				ifFalse: [ seconds < other secondsSinceMidnightUTC ] ]
		ifFalse: [ julianDayNumber < other julianDayNumberUTC ]
]

{ #category : #comparing }
DateAndTime >> = other [ 
	self == other ifTrue: [ ^ true ].
	(self species = other species) ifFalse: [ ^ false ].
	^ self hasEqualTicks: other
]

{ #category : #converting }
DateAndTime >> asDate [

	^ Date starting: self
]

{ #category : #converting }
DateAndTime >> asDateAndTime [

	^ self
]

{ #category : #converting }
DateAndTime >> asDosTimestamp [

	^ (DosTimestamp fromDateAndTime: self) value.
]

{ #category : #converting }
DateAndTime >> asDuration [
	"Answer the duration since midnight."

	^ Duration seconds: self secondsSinceMidnightLocalTime nanoSeconds: nanos
]

{ #category : #converting }
DateAndTime >> asFileNameCompatibleString [
	| stream |
	stream := String new writeStream.
	self printYMDOn: stream.
	stream << '.'.
	self printHMSWithDashesOn: stream.
	^ stream contents
]

{ #category : #converting }
DateAndTime >> asLocal [
	
	^ (self offset = self class localOffset)
		ifTrue: [self]
		ifFalse: [self offset: self class localOffset]
]

{ #category : #converting }
DateAndTime >> asMonth [

	^ Month starting: self
]

{ #category : #converting }
DateAndTime >> asNanoSeconds [
	"Answer the number of nanoseconds since midnight"

	^ self asDuration asNanoSeconds
]

{ #category : #converting }
DateAndTime >> asSeconds [
 	"Return the number of seconds since the Squeak epoch"
 
 	^ (self - (self class epoch)) asSeconds
]

{ #category : #converting }
DateAndTime >> asTime [

	^ Time seconds: self secondsSinceMidnightLocalTime nanoSeconds: nanos
]

{ #category : #converting }
DateAndTime >> asTimeUTC [

	^ Time seconds: self secondsSinceMidnightUTC nanoSeconds: nanos
]

{ #category : #converting }
DateAndTime >> asUTC [

	^ offset isZero
		ifTrue: [ self ]
		ifFalse: [ self offset: 0 ]
]

{ #category : #converting }
DateAndTime >> asUnixTime [
	"answer number of seconds since unix epoch (midnight Jan 1, 1970, UTC)"
	^((self offset: Duration zero) - self class unixEpoch) asSeconds
]

{ #category : #converting }
DateAndTime >> asWeek [

	^ Week starting: self 
]

{ #category : #converting }
DateAndTime >> asYear [

	^ Year starting: self
]

{ #category : #accessing }
DateAndTime >> day [
 
 	^ self dayOfYear
]

{ #category : #enumerating }
DateAndTime >> dayMonthYearDo: aBlock [
	"Return the value of executing block with the Gregorian Calender day, month and year as arguments,
	as computed from my Julian Day Number, julianDayNumber.
	See http://en.wikipedia.org/wiki/Julian_date#Gregorian_calendar_from_Julian_day_number
    
    A short Description for the Constants used below:
    - 400 years span 146097 days in gregorian calendar.
    - 100 years span 36524 days, except every 400 years.
    - 4 years span 1461 days, except every 100 years.
    - 1 year spans 365 days, except every four years
    "

	| l n i j monthDay month fullYear |
	l := self julianDayNumber + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	monthDay := l - (2447 * j // 80).
	l := j // 11.
	month := j + 2 - (12 * l).
	fullYear := 100 * (n - 49) + i + l.

	^ aBlock
		value: monthDay
		value: month
		value: fullYear.
]

{ #category : #accessing }
DateAndTime >> dayOfMonth [
	"Answer which day of the month is represented by the receiver."

	^ self dayMonthYearDo: [ :d :m :y | d ]
]

{ #category : #accessing }
DateAndTime >> dayOfWeek [
	"Sunday=1, ... , Saturday=7"

	^ (self julianDayNumber + 1 rem: 7) + 1
]

{ #category : #accessing }
DateAndTime >> dayOfWeekAbbreviation [

	^ self dayOfWeekName copyFrom: 1 to: 3
]

{ #category : #accessing }
DateAndTime >> dayOfWeekName [

	^ Week nameOfDay: self dayOfWeek
]

{ #category : #accessing }
DateAndTime >> dayOfYear [
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	
	^ self dayMonthYearDo:
		[ :d :m :y | | monthStart |
			monthStart := #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]]
]

{ #category : #accessing }
DateAndTime >> daysInMonth [
	"Answer the number of days in the month represented by the receiver."


	^ self asMonth daysInMonth
]

{ #category : #accessing }
DateAndTime >> daysInYear [
 
 	"Answer the number of days in the year represented by the receiver."
 
 	^ self asYear daysInYear
]

{ #category : #accessing }
DateAndTime >> daysLeftInYear [
 	"Answer the number of days in the year after the date of the receiver."
 
 	^ self daysInYear - self dayOfYear
]

{ #category : #accessing }
DateAndTime >> duration [

	^ Duration zero
]

{ #category : #accessing }
DateAndTime >> firstDayOfMonth [
 
 	^ self asMonth start day
]

{ #category : #private }
DateAndTime >> hasEqualTicks: aDateAndTime [
	
	^ (self julianDayNumberUTC = aDateAndTime julianDayNumberUTC)
		and: [ (seconds = aDateAndTime secondsSinceMidnightUTC)
			and: [ nanos = aDateAndTime nanoSecond ] ]

]

{ #category : #comparing }
DateAndTime >> hash [
	^ (julianDayNumber hashMultiply bitXor: seconds) bitXor: nanos
]

{ #category : #accessing }
DateAndTime >> hour [

	^ self hour24
]

{ #category : #accessing }
DateAndTime >> hour12 [
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	
	^ self hour24 - 1 \\ 12 + 1
]

{ #category : #accessing }
DateAndTime >> hour24 [
	"Answer a number that represents the number of complete hours in the receiver's time part,
	 after the number of complete days has been removed."
 
 	^ self localSeconds // SecondsInHour \\ 24
]

{ #category : #accessing }
DateAndTime >> hours [
 
 	^ self hour
]

{ #category : #accessing }
DateAndTime >> isLeapYear [

	^ Year isLeapYear: self year.
]

{ #category : #accessing }
DateAndTime >> julianDayNumber [
	
	^ julianDayNumber + self julianDayOffset
]

{ #category : #accessing }
DateAndTime >> julianDayNumberUTC [
	
	^ julianDayNumber
]

{ #category : #accessing }
DateAndTime >> julianDayOffset [
	"Return the offset in julian days possibly introduced by the timezone offset"
	
	^ ((seconds + self offset asSeconds) / SecondsInDay) floor
]

{ #category : #accessing }
DateAndTime >> localSeconds [
	" Return the seconds since the epoch in local time."
	^ seconds + self offset asSeconds
]

{ #category : #accessing }
DateAndTime >> meridianAbbreviation [

	^ self asTime meridianAbbreviation
]

{ #category : #maintime }
DateAndTime >> middleOf: aDuration [
	"Return a Timespan where the receiver is the middle of the Duration"

	| duration |
	duration := aDuration asDuration.
	^ Timespan starting: (self - (duration / 2)) duration: duration.
]

{ #category : #maintime }
DateAndTime >> midnight [
	"Answer a DateAndTime starting at midnight (towards the end of the day) local time"
	self dayMonthYearDo: [ :day :month :year|
		^self class 
			year: year
			month: month
			day: day 
			offset: offset ].
]

{ #category : #accessing }
DateAndTime >> minute [
	"Answer a number that represents the number of complete minutes in the receiver' time part, 
	after the number of complete hours has been removed."
	
 	^ self localSeconds // SecondsInMinute \\ 60
]

{ #category : #accessing }
DateAndTime >> minutes [
 
 	^ self minute
]

{ #category : #accessing }
DateAndTime >> month [

	^ self dayMonthYearDo: [ :d :m :y | m ].
]

{ #category : #accessing }
DateAndTime >> monthAbbreviation [

	^ self monthName copyFrom: 1 to: 3
]

{ #category : #accessing }
DateAndTime >> monthIndex [
 
 
 	^ self month
]

{ #category : #accessing }
DateAndTime >> monthName [

	^ Month nameOfMonth: self month
]

{ #category : #accessing }
DateAndTime >> nanoSecond [

	^ nanos
]

{ #category : #accessing }
DateAndTime >> noon [
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: 
		[ :d :m :y | self class year: y month: m day: d hour: 12 minute: 0 second: 0 offset: offset]
]

{ #category : #private }
DateAndTime >> normalizeSecondsAndNanos [
	(NanosInSecond <= nanos or: [ nanos < 0 ])
		ifTrue: [
			seconds := seconds + (nanos // NanosInSecond).
			nanos := nanos \\ NanosInSecond].
	(SecondsInDay <= seconds or: [ seconds < 0 ])
		ifTrue: [
			julianDayNumber := julianDayNumber + (seconds // SecondsInDay).
			seconds := seconds \\ SecondsInDay].
]

{ #category : #accessing }
DateAndTime >> offset [

	^ offset
]

{ #category : #accessing }
DateAndTime >> offset: anOffset [
	"Answer a <DateAndTime> equivalent to the receiver but with its local time 
	being offset from UTC by offset.
	Unlike #translateTo: this will NOT change the absolute in UTC "

	^ self class basicNew 
		ticks: self ticks offset: anOffset asDuration;
		yourself
]

{ #category : #printing }
DateAndTime >> printHMSOn: aStream [
	"Print just hh:mm:ss"

	self printHMSOn: aStream separatedBy: $:
]

{ #category : #printing }
DateAndTime >> printHMSOn: aStream separatedBy: aSeparator [
	"Print just hh<aSep>mm<aSep>ss"

	self hour
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aStream nextPut: aSeparator.
	self minute
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aStream nextPut: aSeparator.
	self second
		printOn: aStream
		base: 10
		length: 2
		padded: true
]

{ #category : #printing }
DateAndTime >> printHMSWithDashesOn: aStream [
	"Print just hh-mm-ss"

	self printHMSOn: aStream separatedBy: $-
]

{ #category : #printing }
DateAndTime >> printMSOn: aStream [
	"Print just mm:ss"
	
	self minute printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $:.
	self second printOn: aStream base: 10 length: 2 padded: true
]

{ #category : #printing }
DateAndTime >> printOn: aStream [
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false
]

{ #category : #printing }
DateAndTime >> printOn: aStream withLeadingSpace: printLeadingSpaceToo [
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	nanos ~= 0 
		ifTrue: [ | n len |
			n := nanos. len := 9.
			[ n \\ 10 = 0 ] whileTrue: [ n := n / 10. len := len - 1 ].
			aStream nextPut: $..
			n printOn: aStream base: 10 length: len padded: true ].
	aStream nextPut: (offset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
	offset hours abs printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $:.
	offset minutes abs printOn: aStream base: 10 length: 2 padded: true.
	offset seconds = 0 
		ifFalse:[ 
			aStream nextPut: $:; print: offset seconds abs truncated ]
]

{ #category : #printing }
DateAndTime >> printSeparateDateAndTimeOn: stream [
	"Print the receiver as separate Date and Time to stream.
	See also #readSeparateDateAndTimeFrom:"

	stream
		print: self asDate;
		space;
		print: self asTime
]

{ #category : #printing }
DateAndTime >> printYMDOn: aStream [
	"Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	^ self printYMDOn: aStream withLeadingSpace: false.
]

{ #category : #printing }
DateAndTime >> printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo [
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ] ].
	year abs printOn: aStream base: 10 length: 4 padded: true.
	aStream nextPut: $-.
	month printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $-.
	day printOn: aStream base: 10 length: 2 padded: true
]

{ #category : #truncation }
DateAndTime >> rounded [
	"Answer a date and time to the nearest whole second"

	^ self species basicNew
		ticks:
			{ julianDayNumber.
			nanos *2 >= NanosInSecond
				ifTrue: [seconds + 1]
				ifFalse: [seconds].
			0 }
		offset: offset
]

{ #category : #accessing }
DateAndTime >> second [
	"Answer a number that represents the number of complete seconds in the receiver's time part, 
	after the number of complete minutes has been removed."
 
 	^ self localSeconds \\ 60
]

{ #category : #accessing }
DateAndTime >> seconds [
 
 	^ self second
]

{ #category : #private }
DateAndTime >> secondsSinceMidnightLocalTime [
	^ self localSeconds \\ SecondsInDay
]

{ #category : #private }
DateAndTime >> secondsSinceMidnightUTC [

	^ seconds
]

{ #category : #private }
DateAndTime >> setJdn: julDays seconds: secs nano: nanoSecs offset: anOffset [
	julianDayNumber := julDays.
	seconds := secs.
	nanos := nanoSecs.
	offset := anOffset.
	self normalizeSecondsAndNanos
]

{ #category : #private }
DateAndTime >> ticks [
	"Private - answer an array with our instance variables. Assumed to be UTC "

	^ Array with: julianDayNumber with: seconds with: nanos.
]

{ #category : #private }
DateAndTime >> ticks: ticks offset: utcOffset [
	"ticks is {julianDayNumber. secondCount. nanoSeconds}"

	self setJdn: (ticks at: 1) seconds: (ticks at: 2) nano: (ticks at: 3) offset: utcOffset
]

{ #category : #accessing }
DateAndTime >> timeZone [
	^ TimeZone offset: self offset
]

{ #category : #accessing }
DateAndTime >> timeZoneAbbreviation [

	^ self timeZone abbreviation
]

{ #category : #accessing }
DateAndTime >> timeZoneName [

	^ self timeZone name
]

{ #category : #enumerating }
DateAndTime >> to: anEnd [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ Timespan starting: self ending: (anEnd asDateAndTime).
]

{ #category : #enumerating }
DateAndTime >> to: anEnd by: aDuration [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (Schedule starting: self ending: (anEnd asDateAndTime))
		schedule: (Array with: aDuration asDuration);
		yourself.
]

{ #category : #enumerating }
DateAndTime >> to: anEnd by: aDuration do: aBlock [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (self to: anEnd by: aDuration) scheduleDo: aBlock
]

{ #category : #offset }
DateAndTime >> translateTo: anOffset [
	"Keep myself's representation and move it to another timezone offset.
	Note that unlike #offset: this WILL change the absolute time in UTC
	
	|t|
	t := DateAndTime now.
	t = (t offset: 2 hours).
	t = (t translateTo: 2 hours).
	" 
	self dayMonthYearDo: [ :day :month :year|
		^ self class
			year: year
			month: month
			day: day
			hour: self hour
			minute: self minute
			second: self second
			nanoSecond: self  nanoSecond
			offset: anOffset asDuration ]
]

{ #category : #offset }
DateAndTime >> translateToUTC [
	" Move this represenation to UTC"
	^ self translateTo: 0 asDuration
]

{ #category : #truncation }
DateAndTime >> truncated [
	"Answer a date and time to the nearest preceding whole second"
	
	^ self species basicNew ticks: { julianDayNumber. seconds. 0 } offset: offset
]

{ #category : #offset }
DateAndTime >> withoutOffset [

	^ self offset: 0
]

{ #category : #accessing }
DateAndTime >> year [
	^ self dayMonthYearDo: [:d :m :y | y ]
]
