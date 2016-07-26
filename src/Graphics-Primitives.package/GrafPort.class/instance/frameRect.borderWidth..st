drawing support
frameRect: rect borderWidth: borderWidth
	sourceX := 0.
	sourceY := 0.
	(rect areasOutside: (rect insetBy: borderWidth)) do:
		[:edgeStrip | self destRect: edgeStrip; copyBits].
