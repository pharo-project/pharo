actions lookup
actionsForPragmas: aSymbolsCollection

	^ (self session class debuggingActionsForPragmas: aSymbolsCollection for: self debugger)
				