generated tests
testIsSpacingCombiningMark

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isSpacingCombiningMark: ch) equals: (charset isSpacingCombiningMark: ch) ]