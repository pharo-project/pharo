private html
openTag: tag attribute: name value: value
	stream
		nextPut: $<;
		nextPutAll: tag;
		space; 
		nextPutAll: name.
	value ifNotNil: [
		stream nextPutAll: '="'.
		self escapeAttributeValue: value asString.
		stream nextPut: $" ].
	stream nextPut: $>