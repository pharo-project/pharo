convenience
fromAllLogsIn: directory
	
	| nodes |
	nodes := directory entries 
		select: [ :each | each name endsWith: OmFileStore defaultFileSuffix ]
		thenCollect: [ :entry | self withEntry: entry ].
		
	^ nodes