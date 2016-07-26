accessing
alpha: anInteger
	"Set the alpha value of the selected color."

	|c|
	c := self selectedColor.
	self selectedColor: (c alpha: anInteger / 255)