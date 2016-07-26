html tags
tag: tag attributes: attributePairs do: block
	"Write an HTML tag with attributes, a collection of alternating keys and values, executing block to generate enclosed content" 
	
	"<tag attr1=""value1"" ... attrN=""valueN""> ... </tag>"

	self openTag: tag attributes: attributePairs.
	block value.
	self closeTag: tag