parsing
timeFrom: aString 
	"Parse the date and time (rfc822) and answer the result as the number of seconds 
	since the start of 1980."
	| s t rawDelta delta plusOrMinus |
	s := aString readStream.

	"date part"
	t := ((self readDateFrom: s) ifNil: [ Date today ]) asSeconds.
	[ s atEnd or: [ s peek isAlphaNumeric ] ] whileFalse: [ s next ].

	"time part"
	s atEnd ifFalse: 
		[ "read time part (interpreted as local, regardless of sender's timezone)"
		s peek isDigit ifTrue: [ t := t + (Time readFrom: s) asSeconds ] ].
	s skipSeparators.

	"Check for a numeric time zone offset"
	('+-' includes: s peek) ifTrue: 
		[ plusOrMinus := s next.
		rawDelta := s peek isDigit 
			ifTrue: [ Integer readFrom: s ]
			ifFalse: [ 0 ].
		delta := (rawDelta // 100 * 60 + (rawDelta \\ 100)) * 60.
		t := plusOrMinus = $+ 
			ifTrue: [ t - delta ]
			ifFalse: [ t + delta ] ].

	"We ignore text time zone offsets like EST, GMT, etc..."
	^ t - (Date year: 1980 day: 1) asSeconds

	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 -500'"
	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 --500'"
	"MailMessage new timeFrom: 'on, 04 apr 2001 14:57:32'"