accessing
summary
	^ String streamContents:
		[:s |
		s nextPutAll: info summaryHeader.
		(dependencies isNil or: [dependencies isEmpty]) ifFalse:
			[s cr; nextPutAll: 'Dependencies: '.
			dependencies
				do: [:ea | s nextPutAll: ea versionInfo name]
				separatedBy: [s nextPutAll: ', ']].
		s cr; cr; nextPutAll: info message]