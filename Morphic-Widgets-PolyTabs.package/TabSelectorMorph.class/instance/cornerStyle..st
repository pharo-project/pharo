rounding
cornerStyle: aSymbol
	"Pass to tabs also."

	super cornerStyle: aSymbol.
	self tabs do: [:t | t cornerStyle: aSymbol]