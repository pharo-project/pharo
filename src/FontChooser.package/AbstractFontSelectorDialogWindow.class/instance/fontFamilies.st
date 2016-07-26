accessing
fontFamilies
	"Answer the set of available fonts families that are supported as Text objects
	in the font that they represent."
	
	^fontFamilies ifNil: [
		self fontFamilies: self defaultFontFamilies.
		fontFamilies]