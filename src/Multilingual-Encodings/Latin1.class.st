"
This class represents the domestic character encoding called ISO-8859-1, also known as Latin-1 used for Most of the Western European Languages.
"
Class {
	#name : #Latin1,
	#superclass : #EncodedCharSet,
	#classInstVars : [
		'rightHalfSequence'
	],
	#category : #'Multilingual-Encodings-CharSets'
}

{ #category : #'class methods' }
Latin1 class >> emitSequenceToResetStateIfNeededOn: aStream forState: state [ 
	state g0Leading ~= 0 ifTrue: 
		[ state charSize: 1.
		state g0Leading: 0.
		state g0Size: 1.
		aStream basicNextPutAll: compoundTextSequence ]

	"Actually, G1 state should go back to ISO-8859-1, too."
]

{ #category : #'class methods' }
Latin1 class >> initialize [
	"
	self initialize
"
	self initializeUcsTable.
	compoundTextSequence := String streamContents: 
		[ :s | 
		s nextPut: (Character value: 27).
		s nextPut: $(.
		s nextPut: $B ].
	rightHalfSequence := String streamContents: 
		[ :s | 
		s nextPut: (Character value: 27).
		s nextPut: $-.
		s nextPut: $A ]
]

{ #category : #'class methods' }
Latin1 class >> initializeUcsTable [
	ucsTable := (0 to: 255) asArray.
]

{ #category : #'accessing - displaying' }
Latin1 class >> isBreakableAt: index in: text [

	| char |
	char := text at: index.
	char = Character space ifTrue: [^ true].
	char = Character cr ifTrue: [^ true].
	char = Character lf ifTrue: [^ true].
	^ false.

]

{ #category : #'character classification' }
Latin1 class >> isCasedLetter: char [
	^ Unicode isCasedLetter: char
]

{ #category : #'character classification' }
Latin1 class >> isClosePunctuation: char [
	^ Unicode isClosePunctuation: char
]

{ #category : #'character classification' }
Latin1 class >> isConnectorPunctuation: char [
^ Unicode isConnectorPunctuation: char
]

{ #category : #'character classification' }
Latin1 class >> isControlOther: char [
^ Unicode isControlOther: char
]

{ #category : #'character classification' }
Latin1 class >> isCurrencySymbol: char [
^ Unicode isCurrencySymbol: char
]

{ #category : #'character classification' }
Latin1 class >> isDashPunctuation: char [
^ Unicode isDashPunctuation: char
]

{ #category : #'character classification' }
Latin1 class >> isDecimalDigit: char [
^ Unicode isDecimalDigit: char
]

{ #category : #'character classification' }
Latin1 class >> isEnclosingMark: char [
	^ Unicode isEnclosingMark: char
]

{ #category : #'character classification' }
Latin1 class >> isFinalQuote: char [
	^ Unicode isFinalQuote: char
]

{ #category : #'character classification' }
Latin1 class >> isFormatOther: char [
	^ Unicode isFormatOther: char
]

{ #category : #'character classification' }
Latin1 class >> isInitialQuote: char [
^ Unicode isInitialQuote: char
]

{ #category : #'character classification' }
Latin1 class >> isLetter: char [
	"Answer whether the receiver is a letter."

	^ Unicode isLetter: char.


]

{ #category : #'character classification' }
Latin1 class >> isLetterModifier: char [
	^ Unicode isLetterModifier: char
]

{ #category : #'character classification' }
Latin1 class >> isLetterNumber: char [
	^ Unicode isLetterNumber: char
]

{ #category : #'character classification' }
Latin1 class >> isLineSeparator: char [
	^ Unicode isLineSeparator: char
]

{ #category : #'character classification' }
Latin1 class >> isLowercase: char [
	"Answer whether the receiver is a lowercase letter.
	The inherited implementation gives the wrong answer for µ and ß,
	and for accented characters."

	| code |
	code := char codePoint.
	(code between: 8r141 and: 8r172) ifTrue: [ ^ true ].
	(char = $÷) ifTrue: [ ^ false ].
	(char = $µ) ifTrue: [ ^ true  ].
	^ code >= 8r337
]

{ #category : #'character classification' }
Latin1 class >> isMathSymbol: char [
^ Unicode isMathSymbol: char
]

{ #category : #'character classification' }
Latin1 class >> isModifierSymbol: char [
^ Unicode isModifierSymbol: char
]

{ #category : #'character classification' }
Latin1 class >> isNonspacingMark: char [
^ Unicode isNonspacingMark: char
]

{ #category : #'character classification' }
Latin1 class >> isOpenPunctuation: char [
^ Unicode isOpenPunctuation: char
]

{ #category : #'character classification' }
Latin1 class >> isOtherLetter: char [
^ Unicode isOtherLetter: char
]

{ #category : #'character classification' }
Latin1 class >> isOtherNumber: char [
	^ Unicode isOtherNumber: char
]

{ #category : #'character classification' }
Latin1 class >> isOtherPunctuation: char [
	^ Unicode isOtherPunctuation: char
]

{ #category : #'character classification' }
Latin1 class >> isOtherSymbol: char [
^ Unicode isOtherSymbol: char
]

{ #category : #'character classification' }
Latin1 class >> isParagraphSeparator: char [
^ Unicode isParagraphSeparator: char
]

{ #category : #'character classification' }
Latin1 class >> isPrivateOther: char [
^ Unicode isPrivateOther: char
]

{ #category : #'character classification' }
Latin1 class >> isSpaceSeparator: char [
^ Unicode isSpaceSeparator: char
]

{ #category : #'character classification' }
Latin1 class >> isSpacingCombiningMark: char [
^ Unicode isSpacingCombiningMark: char
]

{ #category : #'character classification' }
Latin1 class >> isSurrogateOther: char [
^ Unicode isSurrogateOther: char
]

{ #category : #'character classification' }
Latin1 class >> isTitlecaseLetter: char [
^ Unicode isTitlecaseLetter: char
]

{ #category : #'character classification' }
Latin1 class >> isUppercase: char [
	"Answer whether the receiver is an uppercase letter.
	(The inherited implementation gives the wrong answers for 
	accented letters.)"

	(char between: $A and: $Z ) ifTrue: [ ^ true ].
	char = $× ifTrue: [ ^ false ].
	^ char between: $À and: $Þ 

]

{ #category : #'class methods' }
Latin1 class >> leadingChar [

	^ 0.

]

{ #category : #sizing }
Latin1 class >> maxValue [
	"The maximum value of a character in this character set"

	^ 255
]

{ #category : #private }
Latin1 class >> nextPutRightHalfValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state [ 
	state charSize: 1.
	state g1Leading: 0.
	state g1Size: 1.
	aStream basicNextPutAll: rightHalfSequence.
	aStream basicNextPut: (Character value: ascii)
]

{ #category : #'class methods' }
Latin1 class >> nextPutValue: ascii toStream: aStream withShiftSequenceIfNeededForTextConverterState: state [ 
	(ascii <= 127 and: [ state g0Leading ~= 0 ]) ifTrue: 
		[ state charSize: 1.
		state g0Leading: 0.
		state g0Size: 1.
		aStream basicNextPutAll: compoundTextSequence.
		aStream basicNextPut: (Character value: ascii).
		^ self ].
	((128 <= ascii and: [ ascii <= 255 ]) and: [ state g1Leading ~= 0 ]) ifTrue: 
		[ ^ self 
			nextPutRightHalfValue: ascii
			toStream: aStream
			withShiftSequenceIfNeededForTextConverterState: state ].
	aStream basicNextPut: (Character value: ascii).
	^ self
]

{ #category : #'accessing - displaying' }
Latin1 class >> printingDirection [

	^ #right.

]
