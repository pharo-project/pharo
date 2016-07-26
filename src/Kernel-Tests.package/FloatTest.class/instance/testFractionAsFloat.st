tests - conversion
testFractionAsFloat
	"use a random test"
	
	| r m frac err collec |
	r := Random new seed: 1234567.
	m := (2 raisedTo: 54) - 1.
	200 timesRepeat: [
		frac := ((r nextInt: m) * (r nextInt: m) + 1) / ((r nextInt: m) * (r nextInt: m) + 1).
		err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
		self assert: err < (1/2)].
	
	collec := #(16r10000000000000 16r1FFFFFFFFFFFFF 1 2 16r20000000000000 16r20000000000001 16r3FFFFFFFFFFFFF 16r3FFFFFFFFFFFFE 16r3FFFFFFFFFFFFD).
	collec do: [:num |
		collec do: [:den |
			frac := Fraction numerator: num denominator: den.
			err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
			self assert: err <= (1/2)]].