instance creation
fromSeconds: utcSecondsSinceEpoch offset: aUTCOffset
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the given timeZone"

	| integerSeconds nanos |
	integerSeconds := utcSecondsSinceEpoch truncated.
	nanos := integerSeconds = utcSecondsSinceEpoch
		ifTrue: [ 0 ]
		ifFalse: [ ((utcSecondsSinceEpoch - integerSeconds) * NanosInSecond) asInteger ].
	^ self basicNew ticks: {SqueakEpoch . integerSeconds . nanos} offset: aUTCOffset asDuration