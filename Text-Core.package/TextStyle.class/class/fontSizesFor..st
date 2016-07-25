textconstants access
fontSizesFor: aName
	"Answer the pixel sizes for all the fonts in the given text style"

	"TextStyle fontSizesFor: 'Arial'"
	"TextStyle fontSizesFor: 'NewYork'"

	^ (self fontArrayForStyle: aName) collect: [:f | f height ]
