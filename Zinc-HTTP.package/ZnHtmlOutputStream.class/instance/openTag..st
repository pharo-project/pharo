private html
openTag: tag
	stream
		nextPut: $<;
		nextPutAll: tag;
		nextPut: $>