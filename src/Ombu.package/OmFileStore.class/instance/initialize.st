initialize-release
initialize

	super initialize.
	
	entryCount := 0.
	entryPositionsByLocalName := Dictionary new.
	headReference := OmNullReference uniqueInstance.