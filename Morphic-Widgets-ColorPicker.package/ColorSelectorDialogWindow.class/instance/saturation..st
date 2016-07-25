accessing
saturation: anInteger
	"Set the saturation value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color h: c hue s: anInteger / 255 v: c brightness) alpha: c alpha)