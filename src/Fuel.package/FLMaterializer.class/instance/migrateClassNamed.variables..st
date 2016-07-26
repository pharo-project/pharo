configuring-migration
migrateClassNamed: aSymbol variables: aDictionary 

	self 
		migrateClassNamed: aSymbol 
		toClass: (self class environment at: aSymbol)
		variables: aDictionary