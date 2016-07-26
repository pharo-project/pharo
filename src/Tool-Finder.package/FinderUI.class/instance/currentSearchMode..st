accessing
currentSearchMode: aSymbol
	
	self finder searchingStringSilently: self searchingTextArea contentMorph textMorph text asString.
	self searchingTextArea contentMorph textMorph hasUnacceptedEdits:false.
	self finder currentSearchMode: aSymbol.
	.