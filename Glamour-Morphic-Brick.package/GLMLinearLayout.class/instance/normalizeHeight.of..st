height
normalizeHeight: anInteger of: aBrick

	^ (anInteger max: (aBrick layout minHeight brickValue: aBrick))
		min: (aBrick layout maxHeight brickValue: aBrick)