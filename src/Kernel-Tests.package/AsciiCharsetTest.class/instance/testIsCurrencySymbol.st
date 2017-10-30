generated tests
testIsCurrencySymbol

	"Exhaustively check that the charset under test behaves like Unicode"

	| charset |
	charset := self classUnderTest.
	0 to: charset maxValue do: [ :asciiValue | 
		| ch |
		ch := Character value: asciiValue.
		self assert: (Unicode isCurrencySymbol: ch) equals: (charset isCurrencySymbol: ch) ]