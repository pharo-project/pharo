accessing
summaryHeader
	^ String streamContents:
		[:s |
		s
			nextPutAll: 'Name: '; nextPutAll: self name; cr.
		date ifNotNil:
			[s
				nextPutAll: 'Author: '; nextPutAll: author; cr;
				nextPutAll: 'Time: '; nextPutAll:  date asString, ', ', time asString; cr].
		id ifNotNil:
			[s nextPutAll: 'UUID: '; nextPutAll: id asString; cr].
		s
			nextPutAll: 'Ancestors: '; nextPutAll: self ancestorString.
		self stepChildren isEmpty ifFalse:
			[s cr; nextPutAll: 'Backported From: '; nextPutAll: self stepChildrenString].
		]