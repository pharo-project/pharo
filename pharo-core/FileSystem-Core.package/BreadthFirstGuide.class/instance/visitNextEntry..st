showing
visitNextEntry: entry
		
	entry isFile 
		ifTrue: [ ^ visitor visitFile: entry ].
	
	visitor visitDirectory: entry.
	
	(self shouldVisitChildrenOf: entry)
		ifTrue: [ self pushAll: entry reference entries ].