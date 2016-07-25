operations
selectedColor: aColor
	"Set the hue and sv components."

	self aMorph value: aColor alpha.
	self hsvMorph selectedColor: aColor asNontranslucentColor