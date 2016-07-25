as yet unclassified
basicColor: aColor
	"Set the gradient colors."
	
	super color: aColor asNontranslucentColor.
	self
		fillStyle: self gradient