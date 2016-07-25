conversions
httpDate: object
	"Answers a string formatting object as time stamp for HTTP1.1 (RFC 1123). 
	Eg. Sun, 06 Nov 1994 08:49:37 GMT"

	| utc |
	utc := object asDateAndTime asUTC.
	^ String streamContents: [ :stream |
		stream nextPutAll: utc dayOfWeekAbbreviation; nextPut: $,; space.
		stream nextPutAll: (utc dayOfMonth printStringLength: 2 padded: true).
		stream space; nextPutAll: utc monthAbbreviation; space.
		utc year printOn: stream.
		stream space.
		utc printHMSOn: stream.
		stream nextPutAll: ' GMT' ]