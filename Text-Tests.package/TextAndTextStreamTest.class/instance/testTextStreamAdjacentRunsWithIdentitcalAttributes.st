tests
testTextStreamAdjacentRunsWithIdentitcalAttributes

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
