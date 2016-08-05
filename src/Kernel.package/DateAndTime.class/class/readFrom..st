input
readFrom: aStream
	"Parse and return a new DateAndTime instance from stream, 
	as a Date, an optional Time and an optional TimeZone offset.
	The time defaults to midnight, the timezone to the local offset"
	"self readFrom: '2013-03-04T23:47:52.876+01:00' readStream"

	^self readFrom: aStream defaultOffset: self localOffset.