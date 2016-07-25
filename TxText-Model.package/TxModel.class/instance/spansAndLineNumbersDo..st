enumerating
spansAndLineNumbersDo: aBlock
	"This is an optimized way to search for a line. The alternative, querying each span for #lineNumber, could be very expensive because each time we must search the entire text up to that span"
	| lineNumber |
	lineNumber := 1.
	
	self startSpan do: [ :span |
		aBlock value: span value: lineNumber.
		span isLineSeparator ifTrue: [ lineNumber := lineNumber + 1 ] ].