building actions
allActionsForPragmas: aSymbolsCollection
	
	^ (self session class 
		debuggingActionsForPragmas: aSymbolsCollection 
		for: self)

	
