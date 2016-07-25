textconstants access
fontPointSizesFor: aName
	"Answer the point sizes for all the fonts in the given text style"

	"TextStyle fontPointSizesFor: 'Arial'"
	"TextStyle fontPointSizesFor: 'NewYork'"

	^ (self fontArrayForStyle: aName) collect: [:f | f pointSize]
