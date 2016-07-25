drawing support
frameRectRight: rect width: w

	width := 1.
	height := rect height - 1.
	destX := rect right - 1.
	destY := rect top + 1.
	1 to: w do: [:i |
		self copyBits.
		destX := destX - 1.
		destY := destY + 1.
		height := height - 2].
