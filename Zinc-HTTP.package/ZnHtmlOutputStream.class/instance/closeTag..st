private html
closeTag: tag
	stream
		nextPut: $<;
		nextPut: $/;
		nextPutAll: tag;
		nextPut: $>