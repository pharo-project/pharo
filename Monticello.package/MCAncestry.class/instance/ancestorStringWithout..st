ancestry
ancestorStringWithout: packageName
	^ String streamContents:
		[:s | self ancestors do: [:ea | s nextPutAll: (ea nameWithout: packageName)] separatedBy: [s nextPutAll: ', ']]