printing
storeOn: aStream
	aStream nextPut: $(; nextPutAll: self class name;
		nextPutAll: ' rows: '; store: numberOfRows;
		nextPutAll: ' columns: '; store: numberOfColumns;
		nextPutAll: ' contents: '; store: contents;
		nextPut: $)