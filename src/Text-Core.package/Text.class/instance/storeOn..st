printing
storeOn: aStream

	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)