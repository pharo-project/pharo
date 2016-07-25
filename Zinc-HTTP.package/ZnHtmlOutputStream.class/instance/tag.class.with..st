html tags
tag: tag class: cssClass with: string
	"Write an HTML tag with class cssClass, using the escaped string as content" 
	
	"<tag class=""cssClass"">string</tag>"

	self openTag: tag attribute: #class value: cssClass.
	self escape: string.
	self closeTag: tag 