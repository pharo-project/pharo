operations
selectedColor: aColor
	"Set the hue and sv components."

	self hue: aColor hue / 360.
	self svMorph selectedColor: aColor