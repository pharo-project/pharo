convenience
askFileNameAndFileOut: entries in: aDirectory

	| fileNameOrNil outputLog |
	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension)'
		initialAnswer: 'FileOut_', (UUID new asString36 first: 3).

	fileNameOrNil ifNil: [ ^ self ].

	(OmFileStore existsStoreNamed: fileNameOrNil inDirectory: aDirectory)
		ifTrue: [ ^ self inform: 'File already exists' ].

	outputLog := EpLog newWithStore: 
		(OmStoreFactory current named: fileNameOrNil inDirectory: aDirectory).

	self new 
		outputLog: outputLog;
		fileOut: entries.