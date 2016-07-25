private accessing
allHelpPragmas
	 ^(PragmaCollector filter: [:pragma | self pragmaKeywords includes: pragma keyword]) reset collected
