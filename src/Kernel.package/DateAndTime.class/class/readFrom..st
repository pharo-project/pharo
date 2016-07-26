input
readFrom: aStream
	"Parse and return a new DateAndTime instance from stream, 
	as a Date, an optional Time and an optional TimeZone offset.
	The time defaults to midnight, the timezone to the local offset"
	"self readFrom: '2013-03-04T23:47:52.876+01:00' readStream"

	| date time offset |
	date := Date readFrom: aStream.
	[ aStream atEnd or: [ '0123456789Z+-' includes: aStream peek ] ] 
		whileFalse: [ aStream next ].
  	('0123456789' includes: aStream peek)
		ifTrue: [ time := Time readFrom: aStream ]
		ifFalse: [ time := Time midnight ].
	aStream skipSeparators.
	offset := self readTimezoneOffsetFrom: aStream.
	^ self
		year: date year
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset