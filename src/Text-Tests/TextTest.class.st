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

{ #category : #examples }
TextTest >> replacementExample3 [
  "For a Text t, the following assertion should always hold:
     t string size = t run size 
   This test examines the preservation of this assertion for in-place replacement 
   Here, the replacement text is shorteer than the text that is shall replace."
   | replacement startPos length startPosInRep string2 |
   text := (string := 'This is again simple text' copy) asText.
   "Without the copy, we would modify a constant that the compiler attached at the compiled method."
   startPos := string findString: 'simple'. 
   length  := 'simple' size.
   replacement := (string2 := 'both simple and short') asText.
   startPosInRep :=  string2 findString: 'short'.
   text replaceFrom: startPos to: startPos + length - 1 with: replacement startingAt: startPosInRep
]

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
	expectedResult := 'xxxyyy' asText addAttribute: TextEmphasis bold from: 1 to: 3.
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
TextTest >> testAsNumber [
	self assert: '123' asText asNumber equals: 123.
	self assert: '1.23e2' asText asNumber equals: 123.
	self assert: '2r1000' asText asNumber equals: 8.


]

{ #category : #tests }
TextTest >> testAsString [
	self assert: text asString equals: 'Pharo is cool'


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
TextTest >> testHowManyMatch [
	self assert: (text howManyMatch: 'Pharo') equals: 5.
	self assert: (text howManyMatch: 'Pha') equals: 3.
	self assert: (text howManyMatch: 'P') equals: 1.
	self assert: (text howManyMatch: 'p') equals: 0.
	self assert: (text howManyMatch: 'is') equals: 0.
	self assert: (text howManyMatch: 'cool') equals: 0.

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
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'yyyxxx' asText addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.

	"Prepend text with attribute"
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText addAttribute: TextEmphasis italic from: 1 to: 3.
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

{ #category : #running }
TextTest >> testReplaceFromToWith [
	| anotherText bold|
	bold := TextEmphasis bold.
	anotherText := 'hot' asText addAttribute: bold.
	text replaceFrom: 10 to: 13 with: anotherText.
	self assert: text string equals: 'Pharo is hot'.
	self assert: text size equals: 12.
	"check attribute is has expected"
	10 to: 12 do: [ :index | 
		| attr |
		attr := text attributesAt: index.
		self assert: attr size equals: 1.
		self assert: attr first equals: bold ]
]

{ #category : #tests }
TextTest >> testReplacementAtStartPos1 [
	"For a Text t, the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement"
	| replacement  length  |
	text := 'This is a simple text' copy asText.
	"Without the copy, we would modify a constant that the compiler attached at the compiled method."
	length := 'This' size.
	replacement := 'That' asText.
	text replaceFrom: 1 to: length with: replacement startingAt: 1.
	self assert: text string size equals: text runs size
]

{ #category : #tests }
TextTest >> testReplacementAtStartPos2 [
  	"For a Text t, the following assertion should always hold:
     t string size = t run size 
	 This test examines the preservation of this assertion for in-place replacement."
	| replacement  length  |
	text := 'This is a simple text' copy asText.
	"Without the copy, we would modify a constant that the compiler attached at the compiled method."
	length  := 'This' size.
	replacement := 'That********' asText.
	text replaceFrom: 1 to: length with: replacement startingAt: 1.
	self assert: text string size equals: text runs size
]

{ #category : #tests }
TextTest >> testReplacementAtStartPos3 [
	"For a Text t, the following assertion should always hold:
		t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement 
	Here, the replacement text is shorter than the text that is shall replace."
   | text1  replacement  length  |
   text1 := 'This is a simple text' copy asText.
   "Without the copy, we would modify a constant that the compiler attached at the compiled method."
   length  := 'This' size.
   replacement := 'Tht' asText.
  self should: [ text1 replaceFrom: 1 to: length with: replacement startingAt: 1] raise: Error
]

{ #category : #tests }
TextTest >> testRuns [
	| attributes |
	attributes := RunArray new: 13 withAll: TextColor red.
	text runs: attributes.
	
	attributes := RunArray new: 12 withAll: TextColor red.
	self should: [ text runs: attributes ] raise: Error.

	attributes := RunArray new: 14 withAll: TextColor red.
	self should: [ text runs: attributes ] raise: Error.
]

{ #category : #tests }
TextTest >> testSize [
	self assert: text size equals: 13
]

{ #category : #tests }
TextTest >> testString [
	self assert: text string equals: 'Pharo is cool'
]

{ #category : #tests }
TextTest >> testTextReplacement1 [
	"For a Text t, the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement"
   | replacement startPos length startPosInRep string2 |
   text := (string := 'This is a simple text' copy) asText.
   "Without the copy, we would modify a constant that the compiler attached at the compiled method."
   startPos := string findString: 'simple'. 
   length  := 'simple' size.
   replacement := (string2 := 'both simple and short*') asText.
   startPosInRep :=  string2 findString: 'short'.
   text replaceFrom: startPos to: startPos + length - 1 with: replacement startingAt: startPosInRep.
   self assert: text string size equals: text runs size
]

{ #category : #tests }
TextTest >> testTextReplacement2 [
	"For a Text t, the following assertion should always hold:
     	t string size = t run size 
    This test examines the preservation of this assertion for in-place replacement.
    Here, the replacement text has trailing characters."
	| replacement startPos length startPosInRep string2 |
	text := (string := 'This is simple text' copy) asText.
	"Without the copy, we would modify a constant that the compiler attached at the compiled method."
	startPos := string findString: 'simple'. 
	length := 'simple' size.
	replacement := (string2 := 'both simple and short*************') asText.
	startPosInRep :=  string2 findString: 'short'.
	text replaceFrom: startPos to: startPos + length - 1 with: replacement startingAt: startPosInRep.
	self assert: text string size equals: text runs size
]

{ #category : #tests }
TextTest >> testTextReplacement3 [
	"For a Text t, the following assertion should always hold:
		t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement 
	Here, the replacement text is shorteer than the text that is shall replace."

	self should: [ self replacementExample3 ] raise: Error
]

{ #category : #tests }
TextTest >> testUnembellished [
	| anotherText |
	self deny: text unembellished.
	anotherText := 'Hello' asText 
		addAttribute: TextEmphasis bold.
	self assert: anotherText unembellished.
	anotherText addAttribute: TextFontChange font2.
	self deny: anotherText unembellished.

]
