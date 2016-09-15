convenience
fromAllLogsIn: directory
	
	| nodes |
	nodes := directory entries 
		select: [ :each | each name endsWith: OmFileStore defaultFileSuffix ]
		thenCollect: [ :entry | self withEntry: entry ].
	
	nodes 
		do: [ :node | node populateReferencedNodesWith: nodes ]
		displayingProgress: ('Analyzing ', directory asString).
		
	^ nodes