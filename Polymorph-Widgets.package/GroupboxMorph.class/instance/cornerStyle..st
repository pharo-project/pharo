rounding
cornerStyle: aSymbol
	"Pass on to list too."

	super cornerStyle: aSymbol.
	self labelMorph cornerStyle: aSymbol.
	self contentMorph cornerStyle: aSymbol