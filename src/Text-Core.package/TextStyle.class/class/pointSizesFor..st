textconstants access
pointSizesFor: aName
	"Answer all the point sizes for the given text style name"

	"TextStyle pointSizesFor: 'NewYork'"
	^ (self fontArrayForStyle: aName) collect: [:f | f pointSize]
