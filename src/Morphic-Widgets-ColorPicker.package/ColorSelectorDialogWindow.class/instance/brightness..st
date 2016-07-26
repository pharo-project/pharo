accessing
brightness: anInteger
	"Set the brightness value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color h: c hue s: c saturation v: anInteger / 255) alpha: c alpha)