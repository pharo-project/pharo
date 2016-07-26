coloring
green: anInteger
	"Set the green value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: ((Color r: c red * 255 g: anInteger b: c blue * 255 range: 255) alpha: c alpha)