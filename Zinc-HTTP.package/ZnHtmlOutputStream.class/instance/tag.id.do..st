html tags
tag: tag id: cssId do: block
	"Write an HTML tag with id cssId, executing block to generate enclosed content" 
	
	"<tag id=""cssId""> ... </tag>"

	self openTag: tag attribute: #id value: cssId.
	block value.
	self closeTag: tag