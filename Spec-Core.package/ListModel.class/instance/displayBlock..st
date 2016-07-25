api
displayBlock: aBlock
	"<api: #block getter:#displayBlock registration: #whenDisplayBlockChanged:>"
	"Set the one argument block used to wrap your domain specific items.
	The block should return something that can be displayed in a list - like a String or a Text"
	
	wrapBlockHolder value: aBlock