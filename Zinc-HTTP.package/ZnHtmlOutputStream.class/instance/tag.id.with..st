html tags
tag: tag id: cssId with: string
	"Write an HTML tag with id cssId, using the escaped string as content" 
	
	"<tag id=""cssId"">string</tag>"

	self openTag: tag attribute: #id value: cssId.
	self escape: string.
	self closeTag: tag 