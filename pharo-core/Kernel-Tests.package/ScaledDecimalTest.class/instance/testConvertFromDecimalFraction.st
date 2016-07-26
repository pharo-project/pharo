tests
testConvertFromDecimalFraction
	"Converting a Fraction with asScaledDecimal use strictly necessary number of decimal places when possible."
	0 to: 11 do: [:pow2 |
		0 to: 11 do: [:pow5 |
			| fraction sd sd2 |
			fraction := 13 / (2 raisedTo: pow2) / (5 raisedTo: pow5).
			sd := fraction asScaledDecimal.
			self assert: sd scale = (pow2 max: pow5).
			sd2 := ScaledDecimal readFrom: sd printString.
			self assert: sd = sd2]].