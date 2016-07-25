private-writing
rewindData
	super rewindData.
	readDataRemaining := stream size.
	stream position: 0.