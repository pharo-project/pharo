scanning
addLine: newLine

	lines add: newLine.
	extent := (extent x max: newLine width) @ (extent y + newLine height)