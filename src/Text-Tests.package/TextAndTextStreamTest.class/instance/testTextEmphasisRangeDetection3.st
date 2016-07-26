tests
testTextEmphasisRangeDetection3

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