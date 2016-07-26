printing
printOn: aStream
	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self expectedPassCount printString;
		nextPutAll: ' passes, ';
		nextPutAll: self skippedCount printString;
		nextPutAll: ' skipped, ';
		nextPutAll: self expectedDefectCount printString;
		nextPutAll:' expected failures, ';
		nextPutAll: self unexpectedFailureCount printString;
		nextPutAll: ' failures, ';
		nextPutAll: self unexpectedErrorCount printString;
		nextPutAll:' errors, ';
		nextPutAll: self unexpectedPassCount printString;
		nextPutAll:' unexpected passes'.