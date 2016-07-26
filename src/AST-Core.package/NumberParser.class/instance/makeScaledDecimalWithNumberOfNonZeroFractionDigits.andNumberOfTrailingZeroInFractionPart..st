parsing-private
makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart
	"at this point integerPart fractionPart and scale have been read out (in inst var).
	Form a ScaledDecimal.
	Care of eliminating trailing zeroes from the fractionPart"
	
	| decimalMultiplier decimalFraction |
	decimalMultiplier := base raisedToInteger: numberOfNonZeroFractionDigits.
	decimalFraction := integerPart * decimalMultiplier + (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)) / decimalMultiplier.
	neg
		ifTrue: [decimalFraction := decimalFraction negated].
	^decimalFraction asScaledDecimal: scale