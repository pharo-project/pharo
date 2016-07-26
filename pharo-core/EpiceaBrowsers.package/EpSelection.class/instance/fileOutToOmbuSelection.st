operations
fileOutToOmbuSelection

	| fileNameOrNil entries outputLog |
	entries := (self itemsForMenuAction collect: [ :each | each entry ]) reversed.
	
	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension)'
		initialAnswer: 'FileOut'.

	fileNameOrNil ifNil: [ ^ self ].

	outputLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: fileNameOrNil 
				inDirectory: FileSystem workingDirectory).

	EpOmbuExporter new 
		outputLog: outputLog;
		fileOut: entries.