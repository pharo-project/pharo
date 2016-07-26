tests
testConvertFromNonDecimalFraction
	"Converting a Fraction with asScaledDecimal use default number of decimals when the serie of decimals is infinite."
	| defaultNumberOfDecimals |
	defaultNumberOfDecimals := (1/3) asScaledDecimal scale.
	#(6 7 9 11 12 13 14 17 18 19 21 22 23 24) do: [:den |
		| sd sd2 |
		sd := (1/den) asScaledDecimal.
		self assert: sd scale = defaultNumberOfDecimals.
		sd2 := ScaledDecimal readFrom: sd printString.
		self deny: sd = sd2
		] 