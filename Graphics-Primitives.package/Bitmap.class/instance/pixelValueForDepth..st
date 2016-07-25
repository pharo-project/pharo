accessing
pixelValueForDepth: depth
	"Self is being used to represent a single color.  Answer bits that appear in ONE pixel of this color in a Bitmap of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32.  Returns an integer.  First pixel only.  "

	^ (self at: 1) bitAnd: (1 bitShift: depth) - 1