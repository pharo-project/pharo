converting
asApproximateFractionAtOrder: maxOrder
	"Answer a Fraction approximating the receiver. This conversion uses the 
	continued fraction method to approximate a floating point number. If maxOrder
	is zero, use maximum order"

	| num1 denom1 num2 denom2 int frac newD temp order |
	num1 := self asInteger.	"The first of two alternating numerators"
	denom1 := 1.		"The first of two alternating denominators"
	num2 := 1.		"The second numerator"
	denom2 := 0.		"The second denominator--will update"
	int := num1.		"The integer part of self"
	frac := self fractionPart.		"The fractional part of self"
	order := maxOrder = 0 ifTrue: [-1] ifFalse: [maxOrder].
	[frac = 0 or: [order = 0] ]
		whileFalse: 
			["repeat while the fractional part is not zero and max order is not reached"
			order := order - 1.
			newD := 1.0 / frac.			"Take reciprocal of the fractional part"
			int := newD asInteger.		"get the integer part of this"
			frac := newD fractionPart.	"and save the fractional part for next time"
			temp := num2.				"Get old numerator and save it"
			num2 := num1.				"Set second numerator to first"
			num1 := num1 * int + temp.	"Update first numerator"
			temp := denom2.				"Get old denominator and save it"
			denom2 := denom1.			"Set second denominator to first"
			denom1 := int * denom1 + temp.		"Update first denominator"
			10000000000.0 < denom1
				ifTrue: 
					["Is ratio past float precision?  If so, pick which 
					of the two ratios to use"
					num2 = 0.0 
						ifTrue: ["Is second denominator 0?"
								^ Fraction numerator: num1 denominator: denom1].
					^ Fraction numerator: num2 denominator: denom2]].
	"If fractional part is zero, return the first ratio"
	denom1 = 1
		ifTrue: ["Am I really an Integer?"
				^ num1 "Yes, return Integer result"]
		ifFalse: ["Otherwise return Fraction result"
				^ Fraction numerator: num1 denominator: denom1]