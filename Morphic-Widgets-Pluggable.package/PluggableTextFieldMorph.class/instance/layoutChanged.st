initialization
layoutChanged
	super layoutChanged.
	endRow 
		ifNotNil: [ | deltaY |
			deltaY := (self height - endRow fullBounds height) // 2.
			endRow position: self boundsInWorld topRight - (endRow fullBounds width @ deltaY negated)].
