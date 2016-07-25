accessing
summary
	^ String streamContents:
		[:s |
		s
			nextPutAll: self summaryHeader; cr; cr;
			nextPutAll: self message.
		]