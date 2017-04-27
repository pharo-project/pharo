reading
entryPositionsDo: aBlock

	[ self
		nextEntryPositionIfFound: [:position |
			aBlock value: position. true ] 
		ifNone: [ false ]
	] whileTrue