textconstants access
fontWidthsFor: aName
	"Answer the widths for all the fonts in the given text style"

	"TextStyle fontWidthsFor: 'ComicPlain'"
	^ (self fontArrayForStyle: aName) collect: [:f | f maxWidth]
