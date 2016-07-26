coloring
red: anInteger
	"Set the red value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color r: anInteger g: c green * 255 b: c blue * 255 range: 255) alpha: c alpha)