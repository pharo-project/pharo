converting
asFloat
	"Answer a Float that best approximates the value of the receiver.
	This algorithm is optimized to process only the significant digits of a LargeInteger.
	And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below)."
	
	"How numbers are rounded in IEEE 754 default rounding mode:
	A shift is applied so that the highest 53 bits are placed before the floating point to form a mantissa.
	The trailing bits form the fraction part placed after the floating point.
	This fractional number must be rounded to the nearest integer.
	If fraction part is 2r0.1, exactly between two consecutive integers, there is a tie.
	The nearest even integer is chosen in this case.
	Examples (First 52bits of mantissa are omitted for brevity):
	2r0.00001 is rounded downward to 2r0
	2r1.00001 is rounded downward to 2r1
	2r0.1 is a tie and rounded to 2r0 (nearest even)
	2r1.1 is a tie and rounded to 2r10 (nearest even)
	2r0.10001 is rounded upward to 2r1
	2r1.10001 is rounded upward to 2r10
	Thus, if the next bit after floating point is 0, the mantissa is left unchanged.
	If next bit after floating point is 1, an odd mantissa is always rounded upper.
	An even mantissa is rounded upper only if the fraction part is not a tie."
	
	"Algorihm details:
	The floating point hardware can perform the rounding correctly with several excess bits as long as there is a single inexact operation.
	This can be obtained by splitting the mantissa plus excess bits in two part with less bits than Float precision.
	Note 1: the inexact flag in floating point hardware must not be trusted because in some cases the operations would be exact but would not take into account some bits that were truncated before the Floating point operations.
	Note 2: the floating point hardware is presumed configured in default rounding mode."
	
	| mantissa shift excess result n |

	"Check how many bits excess the maximum precision of a Float mantissa."
	excess := self highBitOfMagnitude - Float precision.
	excess > 7
		ifTrue:
			["Remove the excess bits but seven."
			mantissa := self bitShiftMagnitude: 7 - excess.
			shift := excess - 7.
			"An even mantissa with a single excess bit immediately following would be truncated.
			But this would not be correct if above shift has truncated some extra bits.
			Check this case, and round excess bits upper manually."
			((mantissa digitAt: 1) = 2r01000000 and: [self anyBitOfMagnitudeFrom: 1 to: shift])
				ifTrue: [mantissa := mantissa + 1]]
		ifFalse:
			[mantissa := self.
			shift := 0].

	"There will be a single inexact round off at last iteration"
	result := (mantissa digitAt: (n := mantissa digitLength)) asFloat.
	[(n := n - 1) > 0] whileTrue: [
		result := 256.0 * result + (mantissa digitAt: n) asFloat].
	^result timesTwoPower: shift.