accessing
repressedImage: anObject
	"Set the value of repressedImage. This is shown when
	pressed after being off."

	repressedImage := anObject.
	self invalidRect: self bounds