drawing
fillColor: c
	"Note: This always fills, even if the color is transparent."
	self setClearColor: c.
	port fillRect: form boundingBox offset: origin.