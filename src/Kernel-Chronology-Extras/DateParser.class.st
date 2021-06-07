"
Read a Date from the stream based on the pattern which can include the tokens:
	
		y = A year with 1 to n digits (after 2000)
		yy = A year with 2 digits (after 2000)
		yyyy = A year with 4 digits
		m = A month with 1 or 2 digits
		mm = A month with 2 digits
		d = A day with 1 or 2 digits
		dd = A day with 2 digits
		
	...and any other Strings inbetween. Representing $y, $m and $d is done using
	\y, \m and \d and slash itself with \\. Simple example patterns:

		'yyyy-mm-dd'
		'yyyymmdd'
		'yy.mm.dd'
		'y-m-d'
"
Class {
	#name : #DateParser,
	#superclass : #Object,
	#instVars : [
		'inputStream',
		'pattern',
		'patternStream',
		'char',
		'year',
		'month',
		'day',
		'invalidPattern'
	],
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #'instance creation' }
DateParser class >> readingFrom: anInputStream pattern: aPattern [

	"See class comment for pattern description
	self comment"
	
	^self new initializeReadingFrom: anInputStream pattern: aPattern


]

{ #category : #'private - parsing' }
DateParser >> convertTwoDigitsYear [

	(year between: 0 and: 99) ifTrue: [ year := self currentMillenium + year ]
]

{ #category : #'private - parsing' }
DateParser >> createDate [

	^ Date year: year month: month day: day
]

{ #category : #'private - parsing' }
DateParser >> currentMillenium [
	^ (Date current year / 100) asInteger * 100
]

{ #category : #'private - parsing' }
DateParser >> initializeParsing [

	invalidPattern := false.
	patternStream := pattern readStream
]

{ #category : #initialization }
DateParser >> initializeReadingFrom: anInputStream pattern: aPattern [

	inputStream := anInputStream.
	pattern := aPattern.
	
]

{ #category : #'private - parsing day' }
DateParser >> isDayPattern [

	^ char = $d
]

{ #category : #'private - parsing' }
DateParser >> isDoneParsing [

	^ patternStream atEnd or: [ inputStream atEnd or: [  invalidPattern  ]]
]

{ #category : #'private - parsing' }
DateParser >> isEscape [

	^ char = $\
]

{ #category : #'private - parsing year' }
DateParser >> isFourDigitYearPattern [

	^ patternStream nextMatchAll: 'yyy'
]

{ #category : #'private - parsing' }
DateParser >> isInvalidPattern [

	^ year isNil or: [ month isNil or: [ day isNil or: [ invalidPattern ]]]
]

{ #category : #'private - parsing month' }
DateParser >> isMonthPattern [

	^ char = $m
]

{ #category : #'private - parsing day' }
DateParser >> isTwoDigitDayPattern [

	^ patternStream peekFor: $d
]

{ #category : #'private - parsing month' }
DateParser >> isTwoDigitMonthPattern [

	^ patternStream peekFor: $m
]

{ #category : #'private - parsing year' }
DateParser >> isTwoDigitYearPattern [

	^ patternStream peekFor: $y
]

{ #category : #'private - parsing year' }
DateParser >> isYearPattern [

	^ char = $y
]

{ #category : #parsing }
DateParser >> parse [
	^ self parseIfError: [ DateError signal ]
]

{ #category : #'private - parsing' }
DateParser >> parse: aTimeUnitName [
	^ Integer readFrom: inputStream ifFail: [ DateError signal: 'Expecting a ',aTimeUnitName ]
]

{ #category : #'private - parsing' }
DateParser >> parse: timeUnitName expectedSize: anInteger [
	| extractedString result |
	extractedString := inputStream next: anInteger.
	result := extractedString asInteger.
	(result isNil or: [ extractedString size ~= anInteger ]) 
		ifTrue: [ DateError signal: ' Expect a two digit ', timeUnitName, ', got ', extractedString ].
	^ result
]

{ #category : #'private - parsing day' }
DateParser >> parseDay [

	self isTwoDigitDayPattern ifTrue: [ ^ self parseTwoDigitDay ].
	self parseVariableDigitDay
]

{ #category : #'private - parsing' }
DateParser >> parseEscapePattern [

	inputStream next = patternStream next ifFalse: [ invalidPattern := true ]
]

{ #category : #'private - parsing year' }
DateParser >> parseFourDigitYear [

	year := self parse: 'year' expectedSize: 4
]

{ #category : #parsing }
DateParser >> parseIfError: aBlock [
	"Parse the date according to the given pattern"
	self initializeParsing.
	
	[ self isDoneParsing ] whileFalse: [ self parseNextPattern ].

	self isInvalidPattern ifTrue: aBlock.
	self convertTwoDigitsYear.
	
	(inputStream atEnd and: [ patternStream atEnd ]) ifFalse: [ DateError signal: 'Input doesn''t match given pattern.' ].
	
	^ self createDate
]

{ #category : #'private - parsing month' }
DateParser >> parseMonth [

	self isTwoDigitMonthPattern ifTrue: [ ^self parseTwoDigitMonth ].
	self parseVariableDigitMonth 
]

{ #category : #'private - parsing' }
DateParser >> parseNextPattern [

	self readNextChar.

	self isEscape ifTrue: [ ^ self parseEscapePattern ].
	self isYearPattern ifTrue: [ ^ self parseYear ].
	self isMonthPattern ifTrue: [ ^ self parseMonth ].
	self isDayPattern ifTrue: [ ^ self parseDay ].
	
	self parseSameChar
]

{ #category : #'private - parsing' }
DateParser >> parseSameChar [

	inputStream next = char ifFalse: [ invalidPattern := true ]
]

{ #category : #'private - parsing day' }
DateParser >> parseTwoDigitDay [
	day := self parse: 'day' expectedSize: 2
]

{ #category : #'private - parsing month' }
DateParser >> parseTwoDigitMonth [

	month := self parse: 'month' expectedSize: 2
]

{ #category : #'private - parsing year' }
DateParser >> parseTwoDigitYear [

	year := self parse: 'year' expectedSize: 2
]

{ #category : #'private - parsing day' }
DateParser >> parseVariableDigitDay [

	day := self parse: 'day'
]

{ #category : #'private - parsing month' }
DateParser >> parseVariableDigitMonth [

	month := self parse: 'month'
]

{ #category : #'private - parsing year' }
DateParser >> parseVariableDigitYear [

	year := self parse: 'year'
]

{ #category : #'private - parsing year' }
DateParser >> parseYear [

	self isFourDigitYearPattern ifTrue: [ ^ self parseFourDigitYear ].
	self isTwoDigitYearPattern ifTrue: [ ^ self parseTwoDigitYear ].
	self parseVariableDigitYear
]

{ #category : #'private - parsing' }
DateParser >> readNextChar [

	char := patternStream next
]
