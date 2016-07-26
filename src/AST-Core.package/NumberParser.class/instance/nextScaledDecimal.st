parsing-public
nextScaledDecimal
	"Main method for reading a (scaled) decimal number.
	Good Gracious, do not accept a decimal in another base than 10!
	In other words, do not accept radix notation like 2r1.1, even not 10r5.3
	Do not accept exponent notation neither, like 1.0e-3"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart |
	base := 10.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerBase: base.
	(sourceStream peekFor: $.)
		ifTrue: [fractionPart := self nextUnsignedIntegerOrNilBase: base.
			fractionPart ifNil: ["Oops, the decimal point seems not part of this number"
							sourceStream skip: -1.
							^ neg
								ifTrue: [integerPart negated asScaledDecimal: 0]
								ifFalse: [integerPart asScaledDecimal: 0]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifFalse: ["No scale were provided. use number of digits after decimal point as scale"
					scale := nDigits].
			^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart].
	self readScaleWithDefaultNumberOfDigits: 0.
	neg	ifTrue: [integerPart := integerPart negated].
	^integerPart asScaledDecimal: scale