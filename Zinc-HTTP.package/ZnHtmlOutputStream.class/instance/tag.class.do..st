html tags
tag: tag class: cssClass do: block
	"Write an HTML tag with class cssClass, executing block to generate enclosed content" 
	
	"<tag class=""cssClass""> ... </tag>"

	self openTag: tag attribute: #class value: cssClass.
	block value.
	self closeTag: tag