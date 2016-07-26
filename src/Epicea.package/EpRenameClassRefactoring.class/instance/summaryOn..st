printing
summaryOn: aWriteStream
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: oldName;
		nextPutAll: ' ---> ';
		nextPutAll: newName.