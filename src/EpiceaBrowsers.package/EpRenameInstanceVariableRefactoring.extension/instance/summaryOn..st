*EpiceaBrowsers
summaryOn: aWriteStream
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: 'Rename instance variable ';
		print: oldName;
		nextPutAll: ' into ';
		print: newName;
		nextPutAll: ' in ';		
		nextPutAll: affectedClassName.
