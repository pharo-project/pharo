private accessing
allSystemHelpPragmas
	^ (PragmaCollector
		filter: [ :prag | prag selector = self pragmaKeyword ]) reset
		collected