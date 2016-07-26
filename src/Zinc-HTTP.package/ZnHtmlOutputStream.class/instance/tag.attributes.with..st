html tags
tag: tag attributes: attributePairs with: string
	"Write an HTML tag with attributes, a collection of alternating keys and values, using the escaped string as content" 
	
	"<tag attr1=""value1"" ... attrN=""valueN"">string</tag>"

	self openTag: tag attributes: attributePairs.
	self escape: string.
	self closeTag: tag 