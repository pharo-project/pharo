private extraction
extractHelpContentFromString: aString
	| start stop |
	start := aString findString: self helpStartMarker.
	start := start = 0
		ifTrue: [ 1 ]
		ifFalse: [ start + self helpStartMarker size ].
	stop := aString findString: self helpStopMarker startingAt: start.
	stop := stop = 0
		ifTrue: [ aString size ]
		ifFalse: [ stop - 1 ].
	^ (aString copyFrom: start to: stop) trimBoth