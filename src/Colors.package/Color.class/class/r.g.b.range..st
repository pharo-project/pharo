instance creation
r: r g: g b: b range: range
	"Return a color with the given r, g, and b components specified as integers in the range [0..r]. This avoids the floating point arithmetic in the red:green:blue: message and is thus a bit faster for certain applications (such as computing a sequence of colors for a palette)."

	^ self basicNew
		initializeRed: r green: g blue: b range: range;
		yourself 