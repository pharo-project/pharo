accessing
iconNamed: aSymbol
	^ self 
		iconNamed: aSymbol 
		ifNone: [ 
			self isReportingNotFound 
				ifTrue: [ 
					self crLog: (aSymbol, ' icon not found!').
					self notFoundIcon ]
				ifFalse: [ self blankIcon ]]