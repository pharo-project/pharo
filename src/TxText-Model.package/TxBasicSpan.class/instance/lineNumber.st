accessing
lineNumber
	| number |
	number := 1.
	text spansDo: [:span |
		span isLineSeparator ifTrue: [ number := number + 1].
		span == self ifTrue: [ ^ number ].
		].

	self error: 'cannot happen'