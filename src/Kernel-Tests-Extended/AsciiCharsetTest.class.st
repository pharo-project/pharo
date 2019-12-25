"
Tests for the AsciiCharSet class.  The invariant is that AsciiCharSet is  a subset of Unicode, and therefore all of the methods defined there should
have behaviour consisetent with Unicode.
"
Class {
	#name : #AsciiCharsetTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Extended-Charset'
}

{ #category : #helper }
AsciiCharsetTest >> classUnderTest [
	^ AsciiCharset
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsCasedLetter [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isCasedLetter: ch) equals: (charset isCasedLetter: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsClosePunctuation [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isClosePunctuation: ch)
			equals: (charset isClosePunctuation: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsConnectorPunctuation [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isConnectorPunctuation: ch) equals: (charset isConnectorPunctuation: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsControlOther [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isControlOther: ch) equals: (charset isControlOther: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsCurrencySymbol [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isCurrencySymbol: ch) equals: (charset isCurrencySymbol: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsDashPunctuation [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isDashPunctuation: ch) equals: (charset isDashPunctuation: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsDecimalDigit [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isDecimalDigit: ch) equals: (charset isDecimalDigit: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsDigit [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isDigit: ch) equals: (charset isDigit: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsEnclosingMark [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isEnclosingMark: ch) equals: (charset isEnclosingMark: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsFinalQuote [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isFinalQuote: ch) equals: (charset isFinalQuote: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsFormatOther [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isFormatOther: ch) equals: (charset isFormatOther: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsInitialQuote [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isInitialQuote: ch) equals: (charset isInitialQuote: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsLetter [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isLetter: ch) equals: (charset isLetter: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsLetterModifier [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isLetterModifier: ch) equals: (charset isLetterModifier: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsLetterNumber [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isLetterNumber: ch) equals: (charset isLetterNumber: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsLineSeparator [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isLineSeparator: ch) equals: (charset isLineSeparator: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsLowercase [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isLowercase: ch) equals: (charset isLowercase: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsMathSymbol [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isMathSymbol: ch) equals: (charset isMathSymbol: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsModifierSymbol [

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isModifierSymbol: ch) equals: (charset isModifierSymbol: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsNonspacingMark [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isNonspacingMark: ch)
			equals: (charset isNonspacingMark: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsOpenPunctuation [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isOpenPunctuation: ch)
			equals: (charset isOpenPunctuation: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsOtherLetter [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isOtherLetter: ch)
			equals: (charset isOtherLetter: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsOtherNumber [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isOtherNumber: ch)
			equals: (charset isOtherNumber: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsOtherPunctuation [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isOtherPunctuation: ch)
			equals: (charset isOtherPunctuation: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsOtherSymbol [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isOtherSymbol: ch)
			equals: (charset isOtherSymbol: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsParagraphSeparator [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isParagraphSeparator: ch)
			equals: (charset isParagraphSeparator: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsPrivateOther [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isPrivateOther: ch)
			equals: (charset isPrivateOther: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsSpaceSeparator [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isSpaceSeparator: ch)
			equals: (charset isSpaceSeparator: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsSpacingCombiningMark [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isSpacingCombiningMark: ch)
			equals: (charset isSpacingCombiningMark: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsSurrogateOther [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isSurrogateOther: ch)
			equals: (charset isSurrogateOther: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsTitlecaseLetter [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isTitlecaseLetter: ch)
			equals: (charset isTitlecaseLetter: ch) ]
]

{ #category : #'generated tests' }
AsciiCharsetTest >> testIsUppercase [
	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self
			assert: (Unicode isUppercase: ch)
			equals: (charset isUppercase: ch) ]
]
