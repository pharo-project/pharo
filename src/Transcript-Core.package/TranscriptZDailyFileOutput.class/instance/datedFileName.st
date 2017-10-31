private
datedFileName

	^String streamContents: [ :stream |
		stream 
		nextPutAll: (fileName copyUpToLast: self extensionDelimiter);
		nextPutAll: date yyyymmdd;
		nextPut: self extensionDelimiter;
		nextPutAll: (fileName copyAfterLast: self extensionDelimiter)
	]
	