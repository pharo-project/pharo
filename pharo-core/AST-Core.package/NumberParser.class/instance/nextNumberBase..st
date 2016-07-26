parsing-public
nextNumberBase: b
	"Method for reading a number without radix prefix.
	This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := b.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledIntegerOrFloat]