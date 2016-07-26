setup
cellSize: s 
	cellSize := s.
	cellSize = 1 ifTrue: [ ^ self ].
	colorMap := Color 
		colorMapIfNeededFrom: 32
		to: destForm depth