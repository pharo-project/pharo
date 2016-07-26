accessing
colorSelected: aColor
	"A color has been selected. Set the base color for the alpha channel."

	self aMorph color: aColor.
	self triggerSelectedColor