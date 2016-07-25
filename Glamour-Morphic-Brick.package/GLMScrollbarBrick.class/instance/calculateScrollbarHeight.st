scrollbar-calculations
calculateScrollbarHeight

	^ (((self scrollPane height min: (self scrollPane layout maxHeight brickValue: self scrollPane)) / (self scrollPane band height)) * (self height min: (self scrollPane layout maxHeight brickValue: self scrollPane) )) ceiling.