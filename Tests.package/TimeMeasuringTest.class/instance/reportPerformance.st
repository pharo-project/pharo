as yet unclassified
reportPerformance
	| str |
	str := (MultiByteFileStream fileNamed: 'performanceReports.txt') ascii; wantsLineEndConversion: true; yourself.
	str setToEnd;
		nextPutAll: ' test: ', testSelector;
		nextPutAll: ' time: ', realTime asString; 
		nextPutAll: ' version: ', self versionInformation;
		cr; 
		close