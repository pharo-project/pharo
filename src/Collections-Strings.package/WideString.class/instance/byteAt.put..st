accessing
byteAt: index put: aByte

	| d r w |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	w := (self wordAt: d) bitAnd: ((16rFF<<((4 - r)*8)) bitInvert32).
	w := w + (aByte<<((4 - r)*8)).
	self basicAt: d put: w.
	^ aByte.
