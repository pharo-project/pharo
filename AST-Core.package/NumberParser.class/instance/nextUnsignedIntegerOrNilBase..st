parsing-public
nextUnsignedIntegerOrNilBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or nil if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| nPackets high nDigitsHigh lastNonZeroHigh low |
	"read no more digits than one elementary LargeInteger"
	high :=  self nextElementaryLargeIntegerBase: aRadix.
	nDigits = 0 ifTrue: [^nil].
	
	"Not enough digits to form a LargeInteger, stop iteration"
	high isLarge ifFalse: [^high].

	"We now have to engage arithmetic with LargeInteger
	Decompose the integer in a high and low packets of growing size:"
	nPackets := 1.
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	[
	low := self nextLargeIntegerBase: aRadix nPackets: nPackets .
	high := high * (aRadix raisedToInteger: nDigits) + low.
	lastNonZero = 0 ifFalse: [lastNonZeroHigh := lastNonZero + nDigitsHigh].
	nDigitsHigh := nDigitsHigh + nDigits.
	low isLarge]
		whileTrue: [nPackets := nPackets * 2].

	nDigits := nDigitsHigh.
	lastNonZero := lastNonZeroHigh.
	^high