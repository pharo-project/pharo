tests
testTextEmphasisRangeDetection4

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