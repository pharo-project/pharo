fonts and font indexes
fontOfSize: aHeight
	"See fontIndexOfSize.
	Returns the actual font.  Leading not considered."

	^ fontArray at: (self fontIndexOfSize: aHeight)