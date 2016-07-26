expanding-collapsing
expandItemPath: aNodePath
	(self allNodeMorphs at: 1 ifAbsent: [^self]) 
		openItemPath: aNodePath