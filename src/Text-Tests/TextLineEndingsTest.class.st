"
This is a test case for Text>>withSqueakLineEndings and String>>withSqueakLineEndings.

The main problem we've seen with the Text version is that it doesn't preserve formatting correctly.
"
Class {
	#name : #TextLineEndingsTest,
	#superclass : #TestCase,
	#category : #'Text-Tests'
}

{ #category : #tests }
TextLineEndingsTest >> testDecoratedTextConversionCrLF [

	| text newText |
	text := ('123456', String crlf, '901234') asText.
	text addAttribute: TextColor blue from: 4 to: 10.
	text addAttribute: TextColor red from: 6 to: 9.
	text addAttribute: TextEmphasis bold.
	newText := text withSqueakLineEndings.
	self assert: ((text size - 1) = newText size).
	self assert: (newText size = newText runs size).
	self assert: (newText attributesAt: 6) = (text attributesAt: 6).
	self assert: (newText attributesAt: 8) = (text attributesAt: 9).
]

{ #category : #tests }
TextLineEndingsTest >> testDecoratedTextConversionJustLF [

	| text newText |
	text := ('123456', String lf, '901234') asText.
	text addAttribute: TextColor blue from: 4 to: 10.
	text addAttribute: TextColor red from: 6 to: 9.
	text addAttribute: TextEmphasis bold.
	newText := text withSqueakLineEndings.
	self assert: ((text size) = newText size).
	self assert: (newText size = newText runs size).
	self assert: (newText attributesAt: 6) = (text attributesAt: 6).
	self assert: (newText attributesAt: 8) = (text attributesAt: 8).
]

{ #category : #tests }
TextLineEndingsTest >> testDecoratedTextConversionNoLF [

	| text newText |
	text := ('123456', String cr, '901234') asText.
	text addAttribute: TextColor blue from: 4 to: 10.
	text addAttribute: TextColor red from: 6 to: 9.
	text addAttribute: TextEmphasis bold.
	newText := text withSqueakLineEndings.
	self assert: ((text size) = newText size).
	self assert: (newText size = newText runs size).
	self assert: (newText attributesAt: 6) = (text attributesAt: 6).
	self assert: (newText attributesAt: 8) = (text attributesAt: 8).
]

{ #category : #tests }
TextLineEndingsTest >> testSimpleTextConversionCrLF [

	| string newText |
	string := 'This is a test', String crlf, 'of the conversion'.
	newText := string asText withSqueakLineEndings.
	self assert: ((string size - 1) = newText size).
	self assert: (newText size = newText runs size).
]

{ #category : #tests }
TextLineEndingsTest >> testSimpleTextConversionJustCR [

	| string newText |
	string := 'This is a test', String cr, 'of the conversion'.
	newText := string asText withSqueakLineEndings.
	self assert: ((string size) = newText size).
	self assert: (newText size = newText runs size).
]

{ #category : #tests }
TextLineEndingsTest >> testSimpleTextConversionJustLF [

	| string newText |
	string := 'This is a test', String lf, 'of the conversion'.
	newText := string asText withSqueakLineEndings.
	self assert: ((string size) = newText size).
	self assert: (newText size = newText runs size).
]

{ #category : #tests }
TextLineEndingsTest >> testStringConversionCrLF [

	| string newString |
	string := 'This is a test', String crlf, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: ((string size - 1) = newString size).
]

{ #category : #tests }
TextLineEndingsTest >> testStringConversionJustLF [

	| string newString |
	string := 'This is a test', String lf, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: (string size = newString size).
]

{ #category : #tests }
TextLineEndingsTest >> testStringConversionNoLF [

	| string newString |
	string := 'This is a test', String cr, 'of the conversion'.
	newString := string withSqueakLineEndings.
	self assert: (string = newString).
]
