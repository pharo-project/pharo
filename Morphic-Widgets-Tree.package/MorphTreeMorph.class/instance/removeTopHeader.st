updating
removeTopHeader
	topHeader 
		ifNotNil: [self removeMorph: topHeader.
		topHeader := nil]
