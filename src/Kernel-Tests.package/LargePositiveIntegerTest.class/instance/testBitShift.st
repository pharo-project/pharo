tests
testBitShift

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) == i]].