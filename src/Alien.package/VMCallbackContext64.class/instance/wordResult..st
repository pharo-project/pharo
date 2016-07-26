accessing
wordResult: aValue
	"Accept any value in the -2^63 to 2^64-1 range or booleans."
	| anInteger |
	anInteger := aValue isInteger
						ifTrue: [aValue]
						ifFalse:  "Will provoke a mustBeBoolean error if not a boolean."
							[aValue ifTrue: [1] ifFalse: [0]].
	anInteger >= 0
		ifTrue: [self unsignedLongLongAt: 49 put: anInteger]
		ifFalse: [self signedLongLongAt: 49 put: anInteger].
	^1