converting
asScaledDecimal
	"The number of significant digits of the answer is the same as the number of decimal digits in the receiver."
	^ ScaledDecimal newFromNumber: self scale: 0