printing
summaryOn: aWriteStream
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: affectedClassName;
		nextPutAll: ': ';
		nextPutAll: oldName;
		nextPutAll: ' ---> ';
		nextPutAll: newName.