rounding
cornerStyle: aSymbol
	"Pass on to selector and content too."

	super cornerStyle: aSymbol.
	self tabSelectorMorph cornerStyle: aSymbol.
	self contentMorph cornerStyle: aSymbol