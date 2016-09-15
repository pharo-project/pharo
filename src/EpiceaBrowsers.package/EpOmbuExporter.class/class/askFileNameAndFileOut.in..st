convenience
askFileNameAndFileOut: entries in: aDirectory

	| fileNameOrNil outputLog |
	fileNameOrNil := UIManager default
		request: 'Enter a file name (without extension)'
		initialAnswer: 'FileOut_', (UUID new asString36 first: 3).

	fileNameOrNil ifNil: [ ^ self ].

	outputLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: fileNameOrNil 
				inDirectory: aDirectory).

	self new 
		outputLog: outputLog;
		fileOut: entries.