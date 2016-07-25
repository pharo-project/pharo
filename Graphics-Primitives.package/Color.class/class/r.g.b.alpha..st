instance creation
r: r g: g b: b alpha: alpha
"Return a color with the given r, g, and b components in the range [0.0..1.0]."
	^ self basicNew
		initializeRed: r green: g blue: b alpha: alpha ;
		yourself.