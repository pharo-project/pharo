accessing
smoothOrSegmentedPhrase
	| lineName |
	lineName := (closed
					ifTrue: ['outline']
					ifFalse: ['line']) translated.
	^ self isCurve
		ifTrue: [ 'make segmented {1}' translated format: {lineName} ]
		ifFalse: [ 'make smooth {1}' translated format: {lineName} ].