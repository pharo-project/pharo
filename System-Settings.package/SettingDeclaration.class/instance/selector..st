accessing
selector: aSymbol
	self getSelector: aSymbol.
	self setSelector: (aSymbol, ':') asSymbol.