private
newStoreNamed: aGlobalName in: aDirectory

	^ OmBlockFileStore
		named: aGlobalName
		inDirectory: aDirectory