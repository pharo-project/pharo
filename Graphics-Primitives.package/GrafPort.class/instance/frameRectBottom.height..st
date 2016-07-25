drawing support
frameRectBottom: rect height: h

	destX := rect left + 1.
	destY := rect bottom - 1.
	width := rect width - 2.
	height := 1.
	1 to: h do: [:i |
		self copyBits.
		destX := destX + 1.
		destY := destY - 1.
		width := width - 2].
