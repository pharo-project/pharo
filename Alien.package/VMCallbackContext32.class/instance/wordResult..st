accessing
wordResult: aValue
	"Accept any value in the -2^31 to 2^32-1 range or booleans."
	| anInteger |
	anInteger := aValue isInteger
						ifTrue: [aValue]
						ifFalse:  "Will provoke a mustBeBoolean error if not a boolean."
							[aValue ifTrue: [1] ifFalse: [0]].
	anInteger >= 0
		ifTrue: [self unsignedLongAt: 25 put: anInteger]
		ifFalse: [self signedLongAt: 25 put: anInteger].
	^1