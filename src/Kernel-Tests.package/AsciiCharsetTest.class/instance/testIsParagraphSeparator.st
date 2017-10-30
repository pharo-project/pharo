generated tests
testIsParagraphSeparator

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isParagraphSeparator: ch) equals: (charset isParagraphSeparator: ch) ]