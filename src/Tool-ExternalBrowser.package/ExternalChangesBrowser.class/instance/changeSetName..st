initialization
changeSetName: aFileName

	changeSet := CodeImporter fileNamed: aFileName.
	changeSet parseChunks.
	changes items: changeSet codeDeclarations.