html tags
tag: tag with: string
	"Write an HTML tag using the escaped string as content" 
	
	"<tag>string</tag>"

	self openTag: tag.
	self escape: string.
	self closeTag: tag 