tests
testNNegatedEqualsNComplementedPlusOne
	"Verify that n negated = (n complemented + 1) for single bits"
	| n |
	1 to: 100 do: [:i | n := 1 bitShift: i.
				self assert: n negated = ((n bitXor: -1) + 1)]