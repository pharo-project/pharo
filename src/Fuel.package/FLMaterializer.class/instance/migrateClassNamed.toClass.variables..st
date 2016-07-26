configuring-migration
migrateClassNamed: aSymbol toClass: aClass variables: aDictionary 

	migrations
		at: aSymbol
		put: (FLMigration fromClassNamed: aSymbol toClass: aClass variables: aDictionary)