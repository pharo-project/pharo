printing
printOn: stream
	stream
		nextPutAll: self datedVersion;
		nextPutAll: ' update ';
		nextPutAll: self highestUpdate printString