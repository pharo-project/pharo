coloring
blue: anInteger
	"Set the blue value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color r: c red * 255 g: c green * 255 b: anInteger range: 255) alpha: c alpha)