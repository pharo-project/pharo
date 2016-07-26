printing
printBinaryLiteralOn: stream

	"Floats represent those numbers that can be written in Pharo notation as literals of the form {sign}2r1.{fraction}e{exponent} -- where {sign} is the minus
	sign or is empty, {fraction} is a sequence of 52 bits and {exponent} is an integer from -1022 to 1023 -- as well as those numbers that can be written
	as {sign}2r0.{fraction}e-1022 and some special values. This method prints a Float as such a literal expression, or a message expression in the case of
	the special values. Evaluating the expression always results in a Float with the exact same bit pattern, with the exception of the not-a-number Floats
	which all get printed as 'Float nan'. The following article has a table in the section 'Summary' at the end which enumerates the different bit patterns
	of Floats and how to interpret them, which was used in writing this method: http://steve.hollasch.net/cgindex/coding/ieeefloat.html"

	| signBit exponentBits fractionBits |

	signBit := (self basicAt: 1) bitShift: -31.
	exponentBits := ((self basicAt: 1) bitShift: -20) bitAnd: 2r11111111111.
	fractionBits := (((self basicAt: 1) bitAnd: 2r11111111111111111111) bitShift: 32) bitOr: (self basicAt: 2).

	(exponentBits = 2047) ifTrue: [ 
		(fractionBits = 0) ifTrue: [
			stream nextPutAll: 'Float infinity'.
			(signBit = 1) ifTrue: [ stream nextPutAll: ' negated' ].
		] ifFalse: [
			stream nextPutAll: 'Float nan'.
		].
		^ self
	].

	(signBit = 1) ifTrue: [ stream nextPut: $- ].
	(exponentBits = 0) ifTrue: [ 
		(fractionBits = 0) ifTrue: [
			stream nextPutAll: '0.0'
		] ifFalse: [
			stream nextPutAll: '2r0.'.
			fractionBits printOn: stream base: 2 length: (Float precision - 1) padded: true.
			stream nextPutAll: 'e-1022'.
		].
		^ self
	].

	stream nextPutAll: '2r1.'.
	fractionBits printOn: stream base: 2 length: (Float precision - 1) padded: true.
	stream nextPut: $e.
	(exponentBits - 1023) printOn: stream base: 10.