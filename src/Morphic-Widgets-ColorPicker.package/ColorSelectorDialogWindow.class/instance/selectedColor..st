accessing
selectedColor: aColor
	"Set the value of selectedColor. Update the color selectors."

	self basicSelectedColor: aColor.
	self hsvaMorph selectedColor: aColor