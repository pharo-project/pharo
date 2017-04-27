menu - operations
fileOutSelection

	| entries outputDirectory |
	entries := (self selectedEntryItems collect: [ :each | each entry ]) reversed.
	outputDirectory := log store directory.
	
	EpOmbuExporter askFileNameAndFileOut: entries in: outputDirectory.