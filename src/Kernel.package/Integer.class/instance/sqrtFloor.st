testing
sqrtFloor
	"Return the integer part of the square root of self"

	| guess guessSquared delta |
	guess := 1 bitShift: self highBit + 1 // 2.
	[
		guessSquared := guess * guess.
		delta := guessSquared - self // (guess bitShift: 1).
		delta = 0 ] whileFalse: [
			guess := guess - delta ].
	guessSquared = self ifFalse: [ guess := guess - 1 ].
	^guess