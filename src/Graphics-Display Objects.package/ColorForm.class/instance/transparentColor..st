color manipulation
transparentColor: aColor
	"Make all occurances of the given color transparent.  Note: for colors like black and white, which have two entries in the colorMap, this changes BOTH of them.  Not always what you want."

	self replaceColor: aColor with: Color transparent.
