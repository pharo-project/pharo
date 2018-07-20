Extension { #name : #Integer }

{ #category : #'*NewMethodFinder' }
Integer class >> approvedSelectors [

	^ #(decimalDigitLenght denominqtor digitAt:base: digitSum numerator
	    alignedTo: crossSumBase: reciprocalModulo:
	    & << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert32
	       bitOr: bitShift: bitShiftMagnitude: bitString: bitStringLength bitXor: highBit highBitOfMagnitude lowBit noMask:
	   asCharacterDigit asFraction asHexDigit hex
	   asArray asByteArray asByteArrayOfSize:
	   timesRepeat:
	   gcd:
	   radix:
	   digitLength lastDigit
	   isPowerOfTwo
	   asLargestPowerOfTwo asPowerOfTwo asSmallerPowerOfTwo
	   as21BitSmallInt digitLength hightBit highBitOfMagnitude
	   )
]
