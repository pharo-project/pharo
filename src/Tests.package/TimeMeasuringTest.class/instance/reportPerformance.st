performance
reportPerformance
	| str |
	str := ZnCrPortableWriteStream on: (ZnCharacterWriteStream
			on: (File openForWriteFileNamed: 'performanceReports.txt')
			encoding: 'utf8').
	
	str setToEnd;
		nextPutAll: ' test: ', testSelector;
		nextPutAll: ' time: ', realTime asString; 
		nextPutAll: ' version: ', self versionInformation;
		cr; 
		close