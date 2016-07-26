accessing
allReferencesTo: aSymbol
	^ sendersCache at: aSymbol ifAbsentPut: [ self privateReferencesTo: aSymbol ]