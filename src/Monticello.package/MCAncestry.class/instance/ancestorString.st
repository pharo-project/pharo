ancestry
ancestorString
	^ String streamContents:
		[:s | self ancestors do: [:ea | s nextPutAll: ea name] separatedBy: [s nextPutAll: ', ']]