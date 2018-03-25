Extension { #name : #DateAndTime }

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asDosTimestamp [

	^ (DosTimestamp fromDateAndTime: self) value.

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asFileNameCompatibleString [
	| stream |
	stream := String new writeStream.
	self printYMDOn: stream.
	stream << '.'.
	self printHMSWithDashesOn: stream.
	^ stream contents
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asLocal [
	
	^ (self offset = self class localOffset)
		ifTrue: [self]
		ifFalse: [self offset: self class localOffset]

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asMonth [

	^ Month starting: self

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asNanoSeconds [
	"Answer the number of nanoseconds since midnight"

	^ self asDuration asNanoSeconds

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asSeconds [
 	"Return the number of seconds since the Squeak epoch"
 
 	^ (self - (self class epoch)) asSeconds
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asWeek [

	^ Week starting: self 

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> asYear [

	^ Year starting: self
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> day [
 
 	^ self dayOfYear
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> dayOfMonth [
	"Answer which day of the month is represented by the receiver."

	^ self dayMonthYearDo: [ :d :m :y | d ]
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> dayOfWeek [
	"Sunday=1, ... , Saturday=7"

	^ (self julianDayNumber + 1 rem: 7) + 1
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> dayOfWeekAbbreviation [

	^ self dayOfWeekName copyFrom: 1 to: 3
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> dayOfWeekName [

	^ Week nameOfDay: self dayOfWeek

]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> daysInMonth [
	"Answer the number of days in the month represented by the receiver."


	^ self asMonth daysInMonth
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> daysInYear [
 
 	"Answer the number of days in the year represented by the receiver."
 
 	^ self asYear daysInYear
 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> daysLeftInYear [
 	"Answer the number of days in the year after the date of the receiver."
 
 	^ self daysInYear - self dayOfYear
 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> dosEpoch [
	"Answer a DateAndTime representing the DOS epoch (1 January 1980, midnight UTC)"

	^ self basicNew
		ticks: #(2444240 0 0) offset: Duration zero;
		yourself.
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> duration [

	^ Duration zero
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> firstDayOfMonth [
 
 	^ self asMonth start day
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> fromDosTimestamp: anInteger [

	^ (DosTimestamp on: anInteger) asDateAndTime.
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> fromMethodTimeStamp: aString [
	| stream |
	stream := aString readStream.
	stream
		skipSeparators;
		skipTo: Character space.
	^ self readSeparateDateAndTimeFrom: stream
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> fromSeconds: secondsSinceEpochUTC [
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the seconds in UTC time"
	^ self fromSeconds: secondsSinceEpochUTC offset: self localOffset
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> fromSeconds: utcSecondsSinceEpoch offset: aUTCOffset [
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the given timeZone"

	| integerSeconds nanos |
	integerSeconds := utcSecondsSinceEpoch truncated.
	nanos := integerSeconds = utcSecondsSinceEpoch
		ifTrue: [ 0 ]
		ifFalse: [ ((utcSecondsSinceEpoch - integerSeconds) * NanosInSecond) asInteger ].
	^ self basicNew ticks: {SqueakEpoch . integerSeconds . nanos} offset: aUTCOffset asDuration
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> hour [

	^ self hour24

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> hour12 [
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	
	^ self hour24 - 1 \\ 12 + 1
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> hour24 [
	"Answer a number that represents the number of complete hours in the receiver's time part,
	 after the number of complete days has been removed."
 
 	^ self localSeconds // SecondsInHour \\ 24
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> hours [
 
 	^ self hour
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> isLeapYear [

	^ Year isLeapYear: self year.

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> meridianAbbreviation [

	^ self asTime meridianAbbreviation
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> middleOf: aDuration [
	"Return a Timespan where the receiver is the middle of the Duration"

	| duration |
	duration := aDuration asDuration.
	^ Timespan starting: (self - (duration / 2)) duration: duration.
		
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> minutes [
 
 	^ self minute
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> month [

	^ self dayMonthYearDo: [ :d :m :y | m ].
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> monthAbbreviation [

	^ self monthName copyFrom: 1 to: 3

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> monthIndex [
 
 
 	^ self month
 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> monthName [

	^ Month nameOfMonth: self month

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> noon [
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: 
		[ :d :m :y | self class year: y month: m day: d hour: 12 minute: 0 second: 0 offset: offset]
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> noon [

	^ self now noon
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printHMSOn: aStream [
	"Print just hh:mm:ss"

	self printHMSOn: aStream separatedBy: $:
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printHMSOn: aStream separatedBy: aSeparator [
	
	BasicDatePrinter default printHMS: self separatedBy: aSeparator on: aStream 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printHMSWithDashesOn: aStream [
	"Print just hh-mm-ss"

	self printHMSOn: aStream separatedBy: $-
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printMSOn: aStream [
	"Print just mm:ss"
	
	self minute printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $:.
	self second printOn: aStream base: 10 length: 2 padded: true
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printSeparateDateAndTimeOn: stream [
	"Print the receiver as separate Date and Time to stream.
	See also #readSeparateDateAndTimeFrom:"

	stream
		print: self asDate;
		space;
		print: self asTime
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> printYMDOn: aStream [
	"Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	^ self printYMDOn: aStream withLeadingSpace: false.

]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> readFrom: aStream [
	"Parse and return a new DateAndTime instance from stream, 
	as a Date, an optional Time and an optional TimeZone offset.
	The time defaults to midnight, the timezone to the local offset"
	"self readFrom: '2013-03-04T23:47:52.876+01:00' readStream"

	^self readFrom: aStream defaultOffset: self localOffset.
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> readTimezoneOffsetFrom: stream [
	"Read and return an optional timezone offset in the form of 
	[+|-]hh[[separator]mm[[separator]ss]] or Z from stream as a duration.
	If there is no offset, return the local offset."

	^self readTimezoneOffsetFrom: stream default: self localOffset
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> seconds [
 
 	^ self second
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> secondsSinceMidnightLocalTime [
	^ self localSeconds \\ SecondsInDay
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> secondsSinceMidnightUTC [

	^ seconds
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> timeZone [
	^ TimeZone offset: self offset
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> timeZoneAbbreviation [

	^ self timeZone abbreviation

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> timeZoneName [

	^ self timeZone name

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> to: anEnd [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ Timespan starting: self ending: (anEnd asDateAndTime).

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> to: anEnd by: aDuration [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (Schedule starting: self ending: (anEnd asDateAndTime))
		schedule: (Array with: aDuration asDuration);
		yourself.

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> to: anEnd by: aDuration do: aBlock [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (self to: anEnd by: aDuration) scheduleDo: aBlock

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> today [

	^ self midnight

]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> tomorrow [

	^ self today asDate next asDateAndTime
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> totalSeconds [
	"Answer the total seconds ellapsed since the Squeak epoch: 1 January 1901"

	^ self clock totalSeconds
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> translateToUTC [
	" Move this represenation to UTC"
	^ self translateTo: 0 asDuration 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> truncated [
	"Answer a date and time to the nearest preceding whole second"
	
	^ self species basicNew ticks: { julianDayNumber. seconds. 0 } offset: offset
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> withoutOffset [

	^ self offset: 0 
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime >> year [
	^ self dayMonthYearDo: [:d :m :y | y ]
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> year: year day: dayOfYear [
	"Return a DateAndTime"

	^ self
		year: year
		day: dayOfYear
		hour: 0
		minute: 0
		second: 0
]

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> year: year day: dayOfYear hour: hour minute: minute second: second [

	^ self
		year: year
		day: dayOfYear
		hour: hour
		minute: minute
		second: second
		offset: self localOffset.

]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> year: year month: month day: day [
	"Return a DateAndTime, midnight local time"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0
]

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
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

{ #category : #'*Kernel-Chronology-Extras' }
DateAndTime class >> yesterday [

	^ self today asDate previous asDateAndTime

]
