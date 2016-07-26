public
exists: aPath
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ true ] 
		ifAbsent: [ ^ false ].
	