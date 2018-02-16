"
At May 09, 2003 Tim Olson sent a bug report to the Squeak developers list that inspired me to examine the protocol of TextStream in greater detail.  (The bug that Tim reported was present in Squeak 3.4, it is shown in testExample1.) In a discussion that followed,  Daniel Vainsencher proposed that we should have tests for Text and TextStreams. This class is an attempt to implement that proposal. For Squeak 3.4, some of the test examples fail.
"
Class {
	#name : #TextAndTextStreamTest,
	#superclass : #TestCase,
	#category : #'Text-Tests'
}

{ #category : #examples }
TextAndTextStreamTest >> example1: size [

   | ts text |

  ts := TextStream on: (Text new: size).
  ts  nextPutAll: 'xxxxx' asText.
  ts nextPutAll: ('yyyyy' asText allBold, 'zzzzzzz' asText).
  text := ts contents.
  ^text
  
]

{ #category : #examples }
TextAndTextStreamTest >> example2 [

      | ts text |

  ts := TextStream on: (Text new: 50).
  ts  nextPutAll: 'abc' asText.
  ts nextPutAll: 'def' asText allBold.
  ts nextPutAll: 'ghijk' asText.
  text := ts contents.
  ^text
  
]

{ #category : #examples }
TextAndTextStreamTest >> replacementAtStartExample3 [

   | text1  replacement  length  |

   text1 := 'This is a simple text' copy asText.
    " without the copy, we would modify a constant that the compiler attached at the compiled method. "
   length  := 'This' size.
   replacement := 'Tht' asText.
   text1 replaceFrom: 1 
        to:   length
        with: replacement
        startingAt: 1.

]

{ #category : #examples }
TextAndTextStreamTest >> replacementExample3 [

  " for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
    This test examines the preservation of this assertion for in-place replacement 
 Here, the replacement text is shorteer than the text that is shall replace. "


   | text1 string replacement startPos length startPosInRep string2 |

   text1 := (string := 'This is again simple text' copy) asText.
     " without the copy, we would modify a constant that the compiler attached at the compiled method. "
   startPos := string findString: 'simple'. 
   length  := 'simple' size.
   replacement := (string2 := 'both simple and short') asText.
   startPosInRep :=  string2 findString: 'short'.
   text1 replaceFrom: startPos 
        to: startPos + length - 1
        with: replacement
        startingAt: startPosInRep.
   

]

{ #category : #tests }
TextAndTextStreamTest >> testAddStringToTextStream [

	"It is possible to add a string into a TextStream.
	This test verifies that the created text has text attributes for all its characters. "

	| ts text |
	ts := TextStream on: (Text new: 50).
	ts nextPutAll: 'abc' asText.
	ts nextPutAll: 'def' asText allBold.
	ts nextPutAll: 'ghijk'.
	text := ts contents.
     " now, check the fundamental invariant of a text: "
	self assert: text string size = text runs size.

]

{ #category : #'test-errors' }
TextAndTextStreamTest >> testAddingAttributesBeforeBeg [
	| aText |
	aText := 'foo' asText.
	self should: [aText addAttribute: TextColor red from: 0 to: aText size] raise: Error.
]

{ #category : #'test-errors' }
TextAndTextStreamTest >> testAddingAttributesPastTheEnd [
	| aText |
	aText := 'foo' asText.
	self should: [aText addAttribute: TextColor red from: 2 to: aText size + 1] raise: Error.
]

{ #category : #tests }
TextAndTextStreamTest >> testExampleText1 [
	"Inspired by a bug report from Tim Olson.
	Text attributes are lost when the stream collection is expanded.
	Documented BUG!!!"

    | text1 text2 atts1 atts2 |
	text1 := self example1: 10. " here we will loose the attribute bold "
	text2 := self example1: 50. " here we have a larger buffer and will not loose text attributes "
	atts1 := text1 runs copyFrom: 6 to: 10. 
	atts2 := text2 runs copyFrom: 6 to: 10. 

	self assert: atts1 equals: atts2.
      
]

{ #category : #tests }
TextAndTextStreamTest >> testExampleText2 [
	"a Text looses its attributes when it is reversed "
	
	| text1 text2 |
	text1 := self example2.
	text2 := text1 reversed reversed.
	self assert: text1 runs = text2 runs.
 

]

{ #category : #tests }
TextAndTextStreamTest >> testIncludesSubstring [
	"test that Text correctly checks that the supplied substring is present (case sensitive)"
	self assert: ('testing this string' asText includesSubstring: 'ring').
	self assert: ('éèàôüößäóñíá' asText includesSubstring: '').
	self deny: ('éèàôüößäóñíá' asText includesSubstring: 'a').
	self assert: ('éèàôüößäóñíá' asText includesSubstring: 'ßä').
	self deny: ('kjdsnlksjdf' asText includesSubstring: 'K')
]

{ #category : #tests }
TextAndTextStreamTest >> testIncludesSubstringCaseSensitive [
	"test that Text correctly checks that the supplied substring is present,
	both case sensitive and case insensitive"
	self deny: ('test this string' asText includesSubstring: 'Ring' caseSensitive: true).
	self assert: ('test this string' asText includesSubstring: 'Ring' caseSensitive: false).
	self deny: ('123éàôüöß' asText includesSubstring: '' caseSensitive: false).
	self assert: ('123éàôüöß' asText includesSubstring: 'öß' caseSensitive: true).
	self assert: ('123éàôüöß' asText includesSubstring: 'ÀÔ' caseSensitive: false)
]

{ #category : #'test-errors' }
TextAndTextStreamTest >> testRemovingAttributesBeforeBeg [
	| aText |
	aText := Text string: 'foo' attribute: TextColor red .
	self should: [aText removeAttribute: TextColor red from: 0 to: aText size] raise: Error.
]

{ #category : #'test-errors' }
TextAndTextStreamTest >> testRemovingAttributesPastTheEnd [
	| aText |
	aText := Text string: 'foo' attribute: TextColor red .
	self should: [aText addAttribute: TextColor red from: 2 to: aText size + 1] raise: Error.
]

{ #category : #tests }
TextAndTextStreamTest >> testRunArrayAdjacentMerge [

	"this demonstrates that adjancent runs with equal attributes are merged. "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5.
	self assert: (runArray runs size = 2). 
]

{ #category : #tests }
TextAndTextStreamTest >> testRunArrayInvariant [

	"this verifies that the fundamental invariant of a RunArray is always satisfied. "
	"see comment below"
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert:
       ((1 to: runArray size) allSatisfy:
           [:idx |  | lastIndex lastOffset lastRun lengthOfPreviousRuns |
               runArray at: idx.  " updates the cached values "
               lastIndex := runArray instVarNamed: 'lastIndex'.
               lastRun := runArray instVarNamed: 'lastRun'.
               lastOffset := runArray instVarNamed: 'lastOffset'.
               lengthOfPreviousRuns 
                   := (1 to: lastRun - 1)
                      inject: 0
                       into: [:sum :idx2 | sum + (runArray runs at: idx2)].
               lastIndex = (lastOffset + lengthOfPreviousRuns + 1) 
           ]
       ). 

" This method is a bit tricky. First, it uses Object>>instVarNamed: to access instance variables for which no accessors are defined. The same method is used by the debuggers and by various inspectors.
The assertion itself explains the meaning of the cached values."
]

{ #category : #tests }
TextAndTextStreamTest >> testRunArrayReversal [

  	"this tests the reversal of a  RunArray "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray reversed runs size = 3). 
]

{ #category : #tests }
TextAndTextStreamTest >> testRunArrayRunsAreNotMerged [

	" this demonstrates that different runs are not merged "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray runs size = 3). 
]

{ #category : #tests }
TextAndTextStreamTest >> testRunArrayRunsSize [

 	"this demonstrates that the size of a run array is the sum of the sizes of its runs. "
	| runArray |
	runArray := RunArray new.
  	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray size = 15). 
]

{ #category : #'test-errors' }
TextAndTextStreamTest >> testSettingRunsOfWrongSize [
	| aText newRuns |
	aText := 'foo' asText.
	newRuns := aText runs copyReplaceFrom: 2 to: 1 with: (Array with: TextColor red).
	self should: [aText runs: newRuns] raise: Error.
]

{ #category : #tests }
TextAndTextStreamTest >> testTextEmphasisRangeDetection1 [

	"this tests the detection of the range of a text attribute. "
	| text startPos boldStyle |
	text := 'This is a text with attriute bold for some characters' asText.
	startPos := text findString: 'bold' startingAt: 1.
	text addAttribute: TextEmphasis bold from: startPos to: startPos + 3.
	boldStyle := TextEmphasis bold.

  " uncomment the following statement for examine failures: "
  " -----------------
       (1 to: text size) do:
           [:idx | | range |
              range := text rangeOf: boldStyle startingAt: idx.
             Transcript show: startPos; show: ' -- '; show: idx printString; show: '  '; show: range printString; show: range size printString; show: ((idx between: startPos and: startPos + 3)
                  ifTrue:
                    [range first = startPos & (range size = 4)]
                  ifFalse:
                    [range first = idx & (range size = 0)]) printString; cr.
           ].
    ------------- "

	self assert: 
		((1 to: text size) allSatisfy:
			[:idx | | range |
				range := text rangeOf: boldStyle startingAt: idx.
				(idx between: startPos and: startPos + 3)
					ifTrue: [range first = startPos & (range size = 4)]
					ifFalse: [range first = idx & (range size = 0)]])
]

{ #category : #tests }
TextAndTextStreamTest >> testTextEmphasisRangeDetection2 [

	"this tests the detection of the range of a text attribute.
	Here the searched attribute spans three runs. The objective of the test is whether the entire range is always found."
	| text startPos searchedStyle |
	text := 'This is a text with attriute bold for some characters' asText.
	startPos := text findString: 'bold' startingAt: 1.
	text addAttribute: TextEmphasis bold from: startPos to: startPos + 3.
	text addAttribute: TextEmphasis italic from: startPos - 2 to: startPos + 5.
	searchedStyle := TextEmphasis italic.

  " uncomment the following statement for examine failures: " 
  " -----------------------
       (1 to: text size) do:
           [:idx | | range | 
              range := text rangeOf: searchedStyle startingAt: idx.
             Transcript show: startPos; show: ' -- '; show: idx printString; show: '  '; show: range printString; show: range size printString; show: ((idx between: startPos - 2 and: startPos -2 + 7)
                  ifTrue:
                    [range first = (startPos - 2) & (range size = 8)]
                  ifFalse:
                    [range first = idx & (range size = 0)]) printString; cr.
           ].
   ----------------------- "
	self assert: 
			((1 to: text size) allSatisfy:
				[:idx | | range |
					range := text rangeOf: searchedStyle startingAt: idx.
					(idx between: startPos - 2 and: startPos -2 + 7)
						ifTrue: [range first = (startPos - 2) & (range size = 8)]
						ifFalse: [range first = idx & (range size = 0)]])
]

{ #category : #tests }
TextAndTextStreamTest >> testTextEmphasisRangeDetection3 [

	"this tests the detection of the range of a text attribute.
    Here the searched attribute spans three runs. The the range to be detected begins at text position 1. The objective of the test is whether the entire range is always found."

	| text startPos searchedStyle |
	text := 'This is a text with attriute bold for some characters' asText.
	startPos := text findString: 'bold' startingAt: 1.
	text addAttribute: TextEmphasis bold from: startPos to: startPos + 3.
	text addAttribute: TextEmphasis italic from: 1 to: startPos + 5.
	searchedStyle := TextEmphasis italic.

	" uncomment the following statement to examine failures: "
 	" -----------------------
       (1 to: text size) do:
           [:idx | | range | 
              range := text rangeOf: searchedStyle startingAt: idx.
             Transcript show: startPos;
					show: ' -- ';
					show: idx printString;
					show: '  ';
					show: range printString;
					show: range size printString;
                        show: ' ';
					 show: ((idx between: 1 and: startPos + 5)
                  					ifTrue:
                  					  [range first = 1 & (range size = (startPos + 5))]
                					ifFalse:
                   					 [range first = idx & (range size = 0)]) printString; cr.
           ].
   ----------------------- "
	self assert: 
       ((1 to: text size) allSatisfy:
           [:idx | | range |
              range := text rangeOf: searchedStyle startingAt: idx.
              (idx between: 1 and: startPos + 5)
                  ifTrue:
                    [range first = 1 & (range size = (startPos + 5))]
                  ifFalse:
                    [range first = idx & (range size = 0)]])
]

{ #category : #tests }
TextAndTextStreamTest >> testTextEmphasisRangeDetection4 [

	"this tests the detection of the range of a text attribute.
	Here the searched attribute spans three runs. The the range to be detected extends to the end of the text . The objective of the test is whether the 
	entire range is always found."

	| text startPos searchedStyle |
	text := 'This is a text with attriute bold for some characters' asText.
	startPos := text findString: 'bold' startingAt: 1.
	text addAttribute: TextEmphasis bold from: startPos to: startPos + 3.	
	text addAttribute: TextEmphasis italic from: startPos - 2 to: text size.
	searchedStyle := TextEmphasis italic.

	" uncomment the following statement to examine failures: "
 
 	" -----------------------------------------
       (1 to: text size) do:
           [:idx | | range | 
              range := text rangeOf: searchedStyle startingAt: idx.
             Transcript show: startPos;
					show: ' -- ';
					show: idx printString;
					show: '  ';
					show: range printString;
					show: range size printString;
                        show: ' ';
					 show: ((idx between: startPos - 2 and: text size)
                  			ifTrue:
   			                 [range first = (startPos - 2) & (range size = (text size - (startPos - 2) + 1))]
                  			ifFalse:
 			                 [range first = idx & (range size = 0)]) printString;
					cr.
           ].
   -------------------------------"

	self assert: 
       ((1 to: text size) allSatisfy:
           [:idx | | range |
              range := text rangeOf: searchedStyle startingAt: idx.
              (idx between: startPos - 2 and: text size)
                  ifTrue: [range first = (startPos - 2) & (range size = (text size - (startPos - 2) + 1))]
                  ifFalse: [range first = idx & (range size = 0)]])
]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacement1 [

	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement "

   | text1 string replacement startPos length startPosInRep string2 |
   text1 := (string := 'This is a simple text' copy) asText.
   "without the copy, we would modify a constant that the compiler attached at the compiled method. "
   startPos := string findString: 'simple'. 
   length  := 'simple' size.
   replacement := (string2 := 'both simple and short*') asText.
   startPosInRep :=  string2 findString: 'short'.
   text1 replaceFrom: startPos 
        to: startPos + length - 1
        with: replacement
        startingAt: startPosInRep.
   self assert: text1 string size = text1 runs size.

]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacement2 [


  	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
    	This test examines the preservation of this assertion for in-place replacement.
    	Here, the replacement text has trailing characters. "

	| text1 string replacement startPos length startPosInRep string2 |
	text1 := (string := 'This is simple text' copy) asText.
	"without the copy, we would modify a constant that the compiler attached at the compiled method. "
	startPos := string findString: 'simple'. 
	length := 'simple' size.
	replacement := (string2 := 'both simple and short*************') asText.
	startPosInRep :=  string2 findString: 'short'.
	text1 replaceFrom: startPos 
        to: startPos + length - 1
        with: replacement
        startingAt: startPosInRep.
	self assert: text1 string size = text1 runs size.

]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacement3 [

	"for a Text  t,
	the following assertion should always hold:
	t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement 
	Here, the replacement text is shorteer than the text that is shall replace. "

	self should: [self replacementExample3]  raise: Error
]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacementAtStartPos1 [

	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement "

	| text1  replacement  length  |
	text1 := 'This is a simple text' copy asText.
	"without the copy, we would modify a constant that the compiler attached at the compiled method. "
	length  := 'This' size.
	replacement := 'That' asText.
	text1 replaceFrom: 1 
        to:   length
        with: replacement
        startingAt: 1.
	self assert: text1 string size = text1 runs size.

]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacementAtStartPos2 [

  	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement "

	| text1  replacement  length  |
	text1 := 'This is a simple text' copy asText.
	"without the copy, we would modify a constant that the compiler attached at the compiled method. "
	length  := 'This' size.
	replacement := 'That********' asText.
	text1 replaceFrom: 1 
        to:   length
        with: replacement
        startingAt: 1.
	self assert: text1 string size = text1 runs size.

]

{ #category : #tests }
TextAndTextStreamTest >> testTextReplacementAtStartPos3 [

	"for a Text  t,
	the following assertion should always hold:
	t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement 
	Here, the replacement text is shorteer than the text that is shall replace. "

	self should: [self replacementAtStartExample3] raise: Error
]

{ #category : #tests }
TextAndTextStreamTest >> testTextStreamAdjacentRunsWithIdentitcalAttributes [

	"This test verifies that adjacent runs with identical attributes are coalesced."

	| ts text rangeOfBold |
	ts := TextStream on: (Text new: 50).
	ts nextPutAll: 'abc' asText.
	ts nextPutAll: 'def' asText allBold.
	ts nextPutAll: 'ghijk'.
	text := ts contents.
	rangeOfBold := text find:  TextEmphasis bold.
	text removeAttribute: TextEmphasis bold from: rangeOfBold first to: rangeOfBold last. 
     "now, check that only one run is left and that it has the correct size "
	self assert: text runs runs size = 1 & (text runs size = text string size).

]
