tests
testShiftOneLeftThenRight
	"Shift 1 bit left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) = 1].
