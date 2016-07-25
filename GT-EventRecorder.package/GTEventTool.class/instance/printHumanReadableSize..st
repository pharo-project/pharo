printing
printHumanReadableSize: aTotalNumber
	| humanReadable length unit |
	length := ((aTotalNumber decimalDigitLength / 3) truncated) - 1 max: 0.
	humanReadable := (aTotalNumber / (1024 raisedTo: (length min: 3))) rounded.
	length = 0 ifTrue: [ unit := 'bytes' ].
	length = 1 ifTrue: [ unit := 'KB' ].
	length = 2 ifTrue: [ unit := 'MB' ].
	length = 3 ifTrue: [ unit := 'GB' ].
	^ humanReadable printString, ' ', unit