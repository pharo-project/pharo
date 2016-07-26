accessing
hue: anInteger
	"Set the hue value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color h: (anInteger / 255 * 359) rounded s: c saturation v: c brightness) alpha: c alpha)