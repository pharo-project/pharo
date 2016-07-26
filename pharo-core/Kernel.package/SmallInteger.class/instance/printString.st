printing
printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len := self decimalDigitLength.
	result := String new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result byteAt: i put: 48 + (integer - (next * 10)).
		integer := next].
	^result