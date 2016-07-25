accessing
setWidth: width
	"Round up in case fractional."
	
	self extent: (width @ (font ifNil: [TextStyle defaultFont]) height) ceiling