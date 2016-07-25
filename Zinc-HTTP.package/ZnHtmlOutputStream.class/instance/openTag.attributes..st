private html
openTag: tag attributes: attributes
	stream
		nextPut: $<;
		nextPutAll: tag.
	self renderAttributes: attributes.
	stream nextPut: $>