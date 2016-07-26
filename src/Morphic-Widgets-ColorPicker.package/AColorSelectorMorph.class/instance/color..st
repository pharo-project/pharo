accessing
color: aColor
	"Set the gradient colors."
	
	super color: aColor asNontranslucentColor.
	self fillStyle: self defaultFillStyle