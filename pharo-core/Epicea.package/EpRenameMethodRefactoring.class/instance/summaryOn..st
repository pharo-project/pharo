printing
summaryOn: aWriteStream
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: oldSelector printString;
		nextPutAll: ' ---> ';
		nextPutAll: newSelector printString.