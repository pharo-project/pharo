showing
visitNextEntry: entry
	
	entry isFile
		ifTrue: [ visitor visitFile: entry ]
		ifFalse: [ 
			visitor visitDirectory: entry.
			(self shouldVisitChildrenOf: entry)
				ifTrue: [ self pushAll: entry reference entries ]]