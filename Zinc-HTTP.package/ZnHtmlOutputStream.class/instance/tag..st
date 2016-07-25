html tags
tag: tag
	"Write a standalone, self closing HTML tag"
	
	"<tag/>"
	
	stream
		nextPut: $<;
		nextPutAll: tag;
		nextPut: $/;
		nextPut: $>