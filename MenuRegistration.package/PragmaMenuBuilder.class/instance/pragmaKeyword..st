accessing
pragmaKeyword: aString
	"Set the pragma keyword used to select pragmas (see #pragmaCollector)"
	pragmaKeywords add: aString asSymbol.
	self pragmaCollector reset.