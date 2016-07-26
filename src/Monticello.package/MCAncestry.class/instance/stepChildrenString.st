ancestry
stepChildrenString
	^ String streamContents:
		[:s | self stepChildren do: [:ea | s nextPutAll: ea name] separatedBy: [s nextPutAll: ', ']]