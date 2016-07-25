private
logOfLargestPowerOfTwoDividing: aPositiveInteger
	"Answer the base-2 log of the largest power of two that divides the given integer. For example, the largest power of two that divides 24 is 8, whose log base-2 is 3. Do this efficiently even when the given number is a large integer. Assume that the given integer is > 0."
	"DigitalSignatureAlgorithm new logOfLargestPowerOfTwoDividing: (32 * 3)"

	^aPositiveInteger lowBit - 1