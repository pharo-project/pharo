private
digitRshift: anInteger bytes: b lookfirst: a 
	 "Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."
	| n x r f m digit count i |
	n := 0 - anInteger.
	x := 0.
	f := n + 8.
	i := a.
	m := 255 bitShift: 0 - f.
	digit := self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x := digit bitShift: f "Can't exceed 8 bits".
		i := i - 1.
		digit := self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r := Integer new: i - b neg: self negative.
	count := i.
	x := (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit := self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x) 
			"Avoid values > 8 bits".
		x := digit bitShift: n].
	^r