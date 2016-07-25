color mapping
maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	^Color maskingMap: self depth