printing
printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ] ].
	year abs printOn: aStream base: 10 length: 4 padded: true.
	aStream nextPut: $-.
	month printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $-.
	day printOn: aStream base: 10 length: 2 padded: true