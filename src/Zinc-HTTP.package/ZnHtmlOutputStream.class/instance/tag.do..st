html tags
tag: tag do: block
	"Write an HTML tag, executing block to generate enclosed content" 
	
	"<tag>...</tag>"

	self openTag: tag.
	block value.
	self closeTag: tag