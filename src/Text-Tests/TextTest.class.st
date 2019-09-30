"
A TextTest is a test class for testing the behavior of Text
"
Class {
	#name : #TextTest,
	#superclass : #TestCase,
	#instVars : [
		'text',
		'string'
	],
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextTest >> setUp [
	string := 'Pharo is cool'.
	text :=  string copy asText.
	text addAttribute: TextEmphasis bold from: 1 to: 5.
	text addAttribute: TextColor blue from: 1 to: 5.
	text addAttribute: TextEmphasis underlined from: 7 to: 8.
	text addAttribute: TextColor red from: 7 to: 13.
]

{ #category : #tests }
TextTest >> testAppend [
	| receiver argument result expectedResult |

	"Append text without attribute"
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3.
	result := receiver append: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.
	
	"Append text with attribute" 
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText addAttribute: TextEmphasis italic from: 1 to: 3.
	result := receiver append: argument.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3;
		addAttribute: TextEmphasis italic from: 4 to: 6.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.
]

{ #category : #tests }
TextTest >> testAt [ 
	string withIndexDo: [ :char :index |
		self assert: (text at: index) equals: char]
]

{ #category : #tests }
TextTest >> testAtPut [
	text at: 2 put: $H.
	text at: 3 put: $A.
	text at: 4 put: $R.
	text at: 5 put: $O.
	self assert: text string equals: 'PHARO is cool'
]

{ #category : #tests }
TextTest >> testFindStringStartingAt [
	self assert: (text findString: 'Pharo' startingAt: 1) equals: 1.
	self assert: (text findString: 'cool' startingAt: 1) equals: 10.
	self assert: (text findString: 'Pharo' startingAt: 2) equals: 0.
	
]

{ #category : #tests }
TextTest >> testFindStringStartingAtCaseSensitive [
	self assert: (text findString: 'pharo' startingAt: 1 caseSensitive: false) equals: 1.
	self assert: (text findString: 'pharo' startingAt: 1 caseSensitive: true) equals: 0.

	self assert: (text findString: 'COOL' startingAt: 1  caseSensitive: false) equals: 10.
	self assert: (text findString: 'COOL' startingAt: 1  caseSensitive: true) equals: 0.

	
]

{ #category : #tests }
TextTest >> testLineCount [
	text := Text streamContents: [:str | str << 'Pharo' << Character cr << 'is' << Character cr << 'cool'].
	self assert: text lineCount equals: 3
	
]

{ #category : #tests }
TextTest >> testPrepend [
	| receiver argument result expectedResult |

	"Prepend text without attribute"
	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.

	"Prepend text with attribute"
	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText 
		addAttribute: TextEmphasis italic from: 1 to: 3.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis italic from: 1 to: 3;
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs
	
]

{ #category : #running }
TextTest >> testRangeOfStartingAt [
"	text addAttribute: TextEmphasis bold from: 1 to: 5.
	text addAttribute: TextColor blue from: 1 to: 5.
	text addAttribute: TextEmphasis underlined from: 7 to: 8.
	text addAttribute: TextColor red from: 7 to: 13."
	self assert: (text rangeOf: TextEmphasis bold startingAt: 1) equals: (1 to: 5).
	"even an index at beginning of the range, it returns the whole interval"
	self assert: (text rangeOf: TextEmphasis bold startingAt: 2) equals: (1 to: 5).
	self assert: (text rangeOf: TextEmphasis bold startingAt: 5) equals: (1 to: 5).
	"but not when index is outside the range"
	self deny: (text rangeOf: TextEmphasis bold startingAt: 6) equals: (1 to: 5).
	"search for alternate attribute works too"
	self assert: (text rangeOf: TextColor blue startingAt: 2) equals: (1 to: 5).
	"attributes with different range"
	self assert: (text rangeOf: TextColor red startingAt: 7) equals: (7 to: 13).
	self assert: (text rangeOf: TextEmphasis underlined startingAt: 7) equals: (7 to: 8).
	self assert: (text rangeOf: TextEmphasis underlined startingAt: 8) equals: (7 to: 8).
	self deny: (text rangeOf: TextEmphasis underlined startingAt: 5) equals: (7 to: 8).
]
