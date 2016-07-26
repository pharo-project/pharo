tests
testTextEmphasisRangeDetection1

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