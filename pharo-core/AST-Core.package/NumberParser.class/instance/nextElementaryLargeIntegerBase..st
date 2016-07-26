parsing-large int
nextElementaryLargeIntegerBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading if end of digits or if a LargeInteger is formed.
	Count the number of digits and the position of lastNonZero digit and store them in instVar."

	| value digit char |
	value := 0.
	nDigits := 0.
	lastNonZero := 0.
	[value isLarge or: [(char := sourceStream next) isNil
		or: [digit := char digitValue.
			(0 > digit or: [digit >= aRadix])
				and: [sourceStream skip: -1.
					true]]]]
		whileFalse: [
			nDigits := nDigits + 1.
			0 = digit
				ifFalse: [lastNonZero := nDigits].
			value := value * aRadix + digit].
	^value