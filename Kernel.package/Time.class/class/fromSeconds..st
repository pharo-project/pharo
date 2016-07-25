instance creation
fromSeconds: secondCount
	"Answer an instance of me that is secondCount number of seconds since midnight."

	| integerSeconds nanos |
	integerSeconds := secondCount truncated.
	nanos := integerSeconds = secondCount
		ifTrue: [ 0 ]
		ifFalse: [ ((secondCount - integerSeconds) * NanosInSecond) asInteger ].
	^ self seconds: integerSeconds nanoSeconds: nanos