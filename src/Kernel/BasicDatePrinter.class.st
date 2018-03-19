"
I have the simple task of printing a date, time and DateTime.
I do it in the simplest way possible, my subclasses can extend me, and I will use them.

I only print in a single format, does not matter what format the parameter is.

I use ISO 8601 for formatting Date, Time and DateTime

https://en.wikipedia.org/wiki/ISO_8601
"
Class {
	#name : #BasicDatePrinter,
	#superclass : #Object,
	#category : #'Kernel-Chronology'
}

{ #category : #accessing }
BasicDatePrinter class >> default [
	" I prefer to use any of my subclasses than myself. 
	I am a really basic implementation."
	self subclassesDo: [ :subClass | subClass isAbstract ifFalse: [ ^ subClass new ]].
	^ self new
]

{ #category : #printing }
BasicDatePrinter >> printDate: aDate format: formatArray on: aStream [
	"I print a basic representation of the date in ISO 8601 ex: 	2018-03-08"

	self printYMD: aDate withLeadingSpace: false on: aStream
]

{ #category : #printing }
BasicDatePrinter >> printDateAndTime: aDateTime withLeadingSpace: printLeadingSpaceToo on: aStream [
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMD: aDateTime withLeadingSpace: printLeadingSpaceToo on: aStream.
	aStream nextPut: $T.

	self printHMS: aDateTime separatedBy: $: on: aStream.

	aDateTime nanoSecond ~= 0
		ifTrue: [ | n len |
			n := aDateTime nanoSecond.
			len := 9.
			[ n \\ 10 = 0 ]
				whileTrue: [ n := n / 10.
					len := len - 1 ].
			aStream nextPut: $..
			n
				printOn: aStream
				base: 10
				length: len
				padded: true ].
	aStream
		nextPut:
			(aDateTime offset positive
				ifTrue: [ $+ ]
				ifFalse: [ $- ]).
	aDateTime offset hours abs
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aStream nextPut: $:.
	aDateTime offset minutes abs
		printOn: aStream
		base: 10
		length: 2
		padded: true.
	aDateTime offset seconds = 0
		ifFalse: [ aStream
				nextPut: $:;
				print: aDateTime offset seconds abs truncated ]
]

{ #category : #printing }
BasicDatePrinter >> printHMS: aTime separatedBy: aSeparator on: aStream [
	"Print just hh<aSep>mm<aSep>ss"

	aTime hour printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: aSeparator.
	aTime minute printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: aSeparator.
	aTime second printOn: aStream base: 10 length: 2 padded: true
]

{ #category : #printing }
BasicDatePrinter >> printYMD: aDateTime withLeadingSpace: printLeadingSpaceToo on: aStream [
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	aDateTime dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ] ].
	year abs printOn: aStream base: 10 length: 4 padded: true.
	aStream nextPut: $-.
	month printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $-.
	day printOn: aStream base: 10 length: 2 padded: true
]
