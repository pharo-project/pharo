writing
chunkContents: aBlock
	stream
		cr;
		nextPut: (String streamContents: aBlock);
		cr