width
normalizeWidth: anInteger of: aBrick

	^ (anInteger max: (aBrick layout minWidth brickValue: aBrick))
		min: (aBrick layout maxWidth brickValue: aBrick)