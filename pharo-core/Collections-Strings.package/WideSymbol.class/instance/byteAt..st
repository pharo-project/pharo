accessing
byteAt: index

	| d r |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	^ (self wordAt: d) digitAt: ((4 - r) + 1).
