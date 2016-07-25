accessing
color: aColor 
	"Set the receiver's color."
	super color: aColor.
	originalColor := aColor asColor.
	self updateColor