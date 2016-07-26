scrollbar-calculations
calculateScrollbarTopOffset

	^ (((self height) / (self scrollPane band height)) * (self scrollPane band brickBounds top abs)) floor.