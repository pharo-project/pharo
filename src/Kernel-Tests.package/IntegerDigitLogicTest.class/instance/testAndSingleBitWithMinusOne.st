tests
testAndSingleBitWithMinusOne
	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i)].