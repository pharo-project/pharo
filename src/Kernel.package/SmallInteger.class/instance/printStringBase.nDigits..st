printing
printStringBase: b nDigits: n
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."
	
	| integer next result |
	result := String new: n.
	integer := self.
	n to: 1 by: -1 do: [:i |
		next := integer // b.
		result byteAt: i put: (Character digitValue: (integer - (next * b))).
		integer := next].
	^result