drawing-ovals
fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc
	"Fill the given oval.
	Note: The default implementation does not recognize any enhanced fill styles"
	self fillOval: aRectangle color: aFillStyle asColor borderWidth: bw borderColor: bc