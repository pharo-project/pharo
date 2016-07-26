converting
asTrueFraction
	" Answer a fraction that EXACTLY represents self,
	  a double precision IEEE floating point number.
	  Floats are stored in the same form on all platforms.
	  (Does handle gradual underflow but not NANs.)
	  By David N. Smith with significant performance
	  improvements by Luciano Esteban Notarfrancesco.
	  (Version of 11April97)"
	| signexp positive expPart exp fraction fractionPart signedFraction result zeroBitsCount |
	self isInfinite ifTrue: [self error: 'Cannot represent infinity as a fraction'].
	self isNaN ifTrue: [self error: 'Cannot represent Not-a-Number as a fraction'].


	" Extract the sign and the biased exponent "
	signexp := (self basicAt: 1) bitShift: -20.
	positive := (signexp bitAnd: 16r800) = 0.
	expPart := signexp bitAnd: 16r7FF.

	" Extract fractional part; answer 0 if this is a true 0.0 value "
	fractionPart := (((self basicAt: 1) bitAnd: 16rFFFFF) bitShift: 32)+ (self basicAt: 2).
	( expPart=0 and: [ fractionPart=0 ] ) ifTrue: [ ^ 0  ].

	" Replace omitted leading 1 in fraction unless gradual underflow"
	fraction := expPart = 0
		ifTrue: [fractionPart bitShift: 1]
		ifFalse: [fractionPart bitOr: 16r0010000000000000].
	signedFraction := positive ifTrue: [fraction] ifFalse: [fraction negated].
	
	"Unbias exponent: 16r3FF is bias; 52 is fraction width"
	exp := 16r3FF + 52 - expPart.

	" Form the result. When exp>52, the exponent is adjusted by
	  the number of trailing zero bits in the fraction to minimize
	  the (huge) time otherwise spent in #gcd:. "
	exp negative
		ifTrue: [
			result := signedFraction bitShift: exp negated ]
		ifFalse:	[
			zeroBitsCount := fraction lowBit - 1.
			exp := exp - zeroBitsCount.
			exp <= 0
				ifTrue: [
					zeroBitsCount := zeroBitsCount + exp.
					"exp := 0."   " Not needed; exp not
refernced again "
					result := signedFraction bitShift:
zeroBitsCount negated ]
				ifFalse: [
					result := Fraction
						numerator: (signedFraction
bitShift: zeroBitsCount negated)
						denominator: (1 bitShift:
exp) ] ].

	"Low cost validation omitted after extensive testing"
	"(result asFloat = self) ifFalse: [self error: 'asTrueFraction validation failed']."
	^ result 