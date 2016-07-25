html tags
tag: tag attributes: attributes
	"Write a standalone, self closing HTML tag with attributes, a collection of alternating keys and values"
	
	"<tag attr1=""value1"" ... attrN=""valueN""/>"

	stream
		nextPut: $<;
		nextPutAll: tag.
	self renderAttributes: attributes.
	stream
		nextPut: $/;
		nextPut: $>