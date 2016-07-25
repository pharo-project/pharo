accessing
numberOfLinesBefore
	"Count the number of lines just before the receiver"
	| number |
	number := 1.
	text spansDo: [:span |
		span == self ifTrue: [ ^ number ].
		span isLineSeparator ifTrue: [ number := number + 1].
		].

	self error: 'cannot happen'