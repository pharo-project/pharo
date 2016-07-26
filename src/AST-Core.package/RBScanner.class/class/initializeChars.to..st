class initialization
initializeChars: characters to: aSymbol 
	characters do: [:c | classificationTable at: c asInteger put: aSymbol]